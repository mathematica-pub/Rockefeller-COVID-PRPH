#!/bin/bash

# halt execution if any of the commands fail
set -e

# check that all changes are committed to current branch
if [ -z "$(git status --porcelain)" ]; then
  echo "Working directory is clean"
else
  echo "There are uncommitted changes. Please commit or stash and try again."
  exit 1
fi

check_for_release_branch () {
  for branch in $(git for-each-ref --format='%(refname)' refs/heads/); do
    if [[ $branch == "refs/heads/release"* ]]; then
      echo "Another release branch $branch exists. Do you want to use it for deployment? (Y/N)"
      read -r use_existing_branch
      if [[ $use_existing_branch == "Y" ]]; then
        deployBranch=${branch#"refs/heads/"}
      else
        echo "Please finish it or abort it before starting a new release."
        exit 1
      fi
    fi
  done 
}

get_new_version () {
  . versions.config
  echo "Current version is $version, published on $publish_date."

  printf "What should the new version number be (major.minor.build)?"
  read -r new_version
}

check_for_release_branch

if [[ $use_existing_branch == "" ]]; then
  echo "Is there an existing hotfix branch to deploy from? (Y/N)"
  read -r use_existing_branch
  
  if [[ $use_existing_branch == "Y" ]]; then
    echo "Which branch?"
    git branch
    read -r deployBranch
    git checkout $deployBranch
    get_new_version
  else
    echo "Deploy with updated data only, or features and data?"
    select what_deploy in data_only features_and_data;
    do
      case "$what_deploy" in
            data_only)
                baseBranch="production"
                newBranch="hotfix"
                break
              ;;
            features_and_data)
                baseBranch="develop"
                newBranch="release"
                break
              ;;
      esac
    done
  
    git checkout $baseBranch
    echo "Pulling latest $baseBranch branch."
    git pull
    get_new_version
    deployBranch=$newBranch/$new_version
    echo "Creating branch $deployBranch."
    git flow $newBranch start $new_version
  fi
else
  git checkout $deployBranch
  . versions.config
  new_version=$version
fi

today=$(date +'%m-%d-%Y')
echo "Version number is $new_version, to be published today, $today"

echo "Updating version number and publish date in code."
sed -i "s/$version/$new_version/" versions.config
sed -i "s/$publish_date/$today/" versions.config
if [ -z "$(git status --porcelain)" ]; then
  echo "Version and date haven't changed."
else
  git add versions.config
  git commit -m "update: version and date"
fi

echo "Do you want to update the data? (Y/N)"
read -r update_data
if [[ $update_data == "Y" ]]; then
  echo "Updating data."
  Rscript create_df_dashboard.R
  echo "Committing new data to Git."
  if [ -z "$(git status --porcelain)" ]; then
    echo "Data hasn't changed."
  else
    git add .
    git commit -m "update: data"
  fi
fi

echo "Test locally or deploy to internal testing site?"
select testing_type in locally site;
do
  case "$testing_type" in
        locally)
            set +e
            echo "Testing the app locally. Clicking on the link below (it may take a few seconds). Wait until the app loads fully. Exit using CTRL+C in the terminal. You will then have the option to deploy to the production site."
            Rscript -e "shiny::runApp()"
            set -e
            break
          ;;
        site)
            echo "Deploying to internal testing site and exiting."
            Rscript deploy.R dev
            exit 1
          ;;
  esac
done

printf "After local or internal testing, is the app ready to deploy to the production site? (Y/N):"
read -r deploy

if [ "$deploy" == "Y" ]; then
  echo "Deploying."
  Rscript deploy.R prod
  echo "Please update the HISTORY.md file and then finish the release using instructions in the README."
  echo "Finished."
else
  echo "Exiting without deploying."
fi