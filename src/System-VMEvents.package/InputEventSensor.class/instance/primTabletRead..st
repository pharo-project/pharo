private
primTabletRead: cursorIndex
	"Answer the pen tablet data for the cursor having the given index. Answer nil if there is no pen tablet. The data is:
	1. index of the cursor to which this data applies
	2. timestamp of the last state chance for this cursor
	3., 4., and 5. x, y, and z coordinates of the cursor (z is typically 0)
	6. and 7. xTilt and yTilt of the cursor; (signed)
	8. type of cursor (0 = unknown, 1 = pen, 2 = puck, 3 = eraser)
	9. cursor buttons
	10. cursor pressure, downward
	11. cursor pressure, tangential
	12. flags"

	<primitive: 'primitiveReadTablet' module: 'JoystickTabletPlugin'>
	self primitiveFailed
