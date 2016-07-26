This class represents a very compact representation of a boundary shape. It consists of a number of compressed arrays that can be handled by the balloon engine directly. Due to this, there are certain restrictions (see below). Boundaries are always represented by three subsequent points that define a quadratic bezier segment. It is recommended that for straight line segments the control point is set either to the previous or the next point.

Instance variables:
	points		<PointArray | ShortPointArray>	Point storage area
	leftFills		<ShortRunArray>	Containing the "left" fill index of each segment
	rightFills	<ShortRunArray>	Containing the "right" fill index of each segment
	lineWidths	<ShortRunArray>	Containing the line width of each segment
	lineFills		<ShortRunArray>	Containing the line fill (e.g., line color) of each segment
	fillStyles	<Collections>			Contains the actual fill styles referenced by the indexes

RESTRICTIONS:
None of the ShortRunArrays may contain a run of length Zero.
Also, due to the use of ShortRunArrays 
	a) you cannot have more than 32768 different fill styles
	b) you cannot have a line width that exceeds 32768
In case you have trouble with a), try to merge some of the fills into one. You might do so by converting colors to 32bit pixel values. In case you have trouble with b) you might change the general resolution of the compressed shape to have less accuracy.
