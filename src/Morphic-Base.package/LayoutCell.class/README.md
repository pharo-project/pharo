I am used in table layouts to hold temporary values while the layout is being computed.

Instance variables:
	target 		<Morph>		The morph contained in this cell
	cellSize 		<Point>		The size of the cell
	extraSpace 	<nil | Point>	Additional space to add after this cell
	nextCell 	<nil | LayoutCell>	The next cell in the arrangement.

Implementation note:
Both, cellSize and extraSpace contains points where
	x - represents the primary table direction
	y - represents the secondary table direction
