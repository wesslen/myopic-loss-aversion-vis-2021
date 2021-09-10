import React, { useRef, useEffect } from "react";
import BarChart from "../barchart/barchart";
import Dotplot from "../dotplot/dotplotalt";
import Hops from "../hops/hops";
import PointPlot from "../pointplot/pointplot";
import Interval from "../interval/interval";
import Density from "../density/density";
import Table from "../table/table";

const VizController = (props) => {
  let vizType = props.vizType;

  switch (vizType) {
    case "barchart":
      // code block
      return (
        <BarChart
          title={props.title}
          extent={props.extent}
          allocation={props.allocation}
          data={props.data}
        ></BarChart>
      );
      break;
    case "dotplot":
      // code block
      return (
        <Dotplot
          title={props.title}
          extent={props.extent}
          allocation={props.allocation}
          data={props.data}
          nBins={50}
        ></Dotplot>
      );
      break;
    case "hops":
      return (
        <Hops
          showDist={false}
          title={props.title}
          extent={props.extent}
          allocation={props.allocation}
          data={props.data}
        ></Hops>
      );
      break;
    case "hopsdist":
      return (
        <Hops
          showDist={true}
          title={props.title}
          extent={props.extent}
          allocation={props.allocation}
          data={props.data}
        ></Hops>
      );
      break;
    case "point":
      return (
        <PointPlot
          title={props.title}
          extent={props.extent}
          allocation={props.allocation}
          data={props.data}
        ></PointPlot>
      );
      break;
    case "interval":
      return (
        <Interval
          title={props.title}
          extent={props.extent}
          allocation={props.allocation}
          data={props.data}
        ></Interval>
      );
      break;
    case "density":
      return (
        <Density
          title={props.title}
          extent={props.extent}
          densityExtent={props.densityExtent}
          allocation={props.allocation}
          data={props.data}
        ></Density>
      );
      break;
    case "table":
      return (
        <Table
          title={props.title}
          extent={props.extent}
          allocation={props.allocation}
          data={props.data}
        ></Table>
      );
      break;
    default:
      // code block
      return null;
  }
};

export default VizController;
