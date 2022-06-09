// !preview r2d3 data=data.frame(value = 0.1), css  = "www/styles.css", options( r2d3.shadow = FALSE), options = list( format = ".1%",  tick_format = ".0%", rev = "true", min = 0, max = 1, visible = "true", name = "id", r = 8)

svg.style("background", "transparent");

var margin = {top: 0, right: 20, bottom: 0, left: 15};

const duration = 1500;
var t = d3.transition().ease();

var colors = ['#0B2949', '#D02B27']; // dark blue to red
if (options.rev == "true") { 
  colors = ['#D02B27', '#0B2949'];
}

var r = options.r

svg
  .append("rect")
  .attr("width", "100%")
  .attr("height", "100%")
  .attr("fill", "transparent");
    
svg
  .append('svg:linearGradient')
  .attr('id', ('grad-' + options.name))
  .attr('x1', '0%')
  .attr('x2', '100%')
  .attr('y1', '0%')
  .attr('y2', '0%')
  .selectAll('stop')
  .data(colors)
  .enter()
  .append('stop')
  .style('stop-color', function(d, i){ return d; })
  .attr('offset', function(d, i){
    return 100 * (i / (colors.length - 1)) + '%';
  });

var x = d3.scaleLinear().range([0, width - margin.left - margin.right]); 
  
var fig = svg
  .attr("width", width)
  .attr("height", height)
  .append("g")
  .attr("id", "figure-" + options.name)
  .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
  
var axis = fig.append("g")
    .attr("class", "kpi_xaxis")
    .attr("transform", "translate(0," + (height/4 + 12) + ")");

var tooltip = d3.select("body")
  .append("div")
  .attr("class", "tooltip")
  .attr("position", "absolute")
  .style("opacity", 0); 
  
fig.append('rect')
  .attr('width', width - margin.left - margin.right)
  .attr('height', 10)
  .attr('y', height*0.25)
  .attr('fill', ("url(#grad-" + options.name + ")"))
  .attr("rx", 5); // round edges
  
svg.append("rect")
  .attr('id', 'no-value-rect-' + options.name)
  .attr("width", width)
  .attr("height", height)
  .attr('x', -1)
  .attr("fill", "#F5F1E8");
        
svg.append("text")
  .attr('id', 'no-value-text-' + options.name)
  .attr("x", 2*margin.left)
  .attr("y", (height)*0.25)
  .attr("dy", ".35em")
  .attr("fill", "grey")
  .text("No value for selected inputs.");
  
// ON RENDER -------------------------------------------------------------------

r2d3.onRender(function(data, svg, width, height, options) {
  x.domain([options.min, options.max]);
  var xaxis = d3.axisBottom(x).ticks(5).tickFormat(d3.format(options.tick_format));
  axis.transition(t).call(xaxis);
    
  var point = fig.selectAll("circle.kpi_point").data(data, d => d.value);
  
  // hard code height as 50 if it registers as 0 for some reason
  var pos_y = height === 0 ? 50 : height; 
  
  point
    .enter().append("circle")
    .attr("class", "kpi_point")  
    .style("stroke", "black")
    .attr("r", r)
    .attr("cy", pos_y*0.25 + 5)
    .on("mouseover", function(d, i){
        tooltip.html(d3.format(options.format)(d.value)) 
          .style("opacity", 1);
        d3.select(this).raise()
          .attr("r", r*1.2);
    })
    .on("mousemove", function(d, i){
        tooltip
          .style("left", (d3.event.pageX  + 20 + "px")) 
          .style("top", (d3.event.pageY + 20  + "px"));
    })
    .on("mouseout", function(d, i){
        tooltip.style("opacity", 0);
        d3.select(this).attr("class", "kpi_point")
        .style("stroke", "black")
        .attr("r", r);
    })
    .transition(t)
      .attr("cx", d => x(d.value));
      
  point.transition(t)
      .attr("cx",  d => x(d.value));
      
  point.exit().remove();
  
  if (options.visible == "false"){
    d3.select("#figure-" + options.name).style("visibility", "hidden");
    d3.select("#no-value-rect-" + options.name).style("visibility", "visible");
    d3.select("#no-value-text-" + options.name).style("visibility", "visible");
  } else {
    d3.select("#figure-" + options.name).style("visibility", "visible");
    d3.select("#no-value-rect-" + options.name).style("visibility", "hidden");
    d3.select("#no-value-text-" + options.name).style("visibility", "hidden");
  }
    
});


