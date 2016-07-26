I represent a rectangular area of the screen. Arithmetic functions take points as arguments and carry out scaling and translating operations to create new instances of me. Rectangle functions create new instances by determining intersections of rectangles with rectangles.

Note 1: only rectangles parallel to reference frame (Screen) can be represented by this class.

Note 2: the Rectangle is represented by two extremities of one diagonal. By convention, it must be the diagonal:
	from rectangle origin (the point having smallest coordinates in reference frame),
	to rectangle corner (the point having largest coordinates in reference frame).

Note 3: Screen coordinates conventions are:
	x is horizontal axis, zero at left border, oriented toward right;
	y is vertical axis, zero at top border, oriented toward bottom.
This corresponds to the latin convention for writing text from left to right and top to bottom.

Note 4: the Rectangle extent is obtained by subtracting rectangle origin to rectangle corner coordinates.
If this leads to a negative width (extent x coordinate) and/or a negative height (extent y coordinate), then the Rectangle is degenerated and considered empty.

Instance variables:
	origin	<Point> the coordinates of corner having smallest coordinates (top left in Screen coordinates)
	corner	<Point> the coordinates of corner having largest coordinates (bottom right in Screen coordinates)
