private
primTabletGetParameters: cursorIndex
	"Answer the pen tablet parameters. For parameters that differ from cursor to cursor, answer those associated with the cursor having the given index. Answer nil if there is no pen tablet. The parameters are:
	1. tablet width, in tablet units
	2. tablet height, in tablet units
	3. number of tablet units per inch
	4. number of cursors (pens, pucks, etc; some tablets have more than one)
	5. this cursor index
	6. and 7. x scale and x offset for scaling tablet coordintes (e.g., to fit the screen)
	8. and 9. y scale and y offset for scaling tablet coordintes  (e.g., to fit the screen)
	10. number of pressure levels
	11. presure threshold needed close pen tip switch 
	12. number of pen tilt angles"

	<primitive: 'primitiveGetTabletParameters' module: 'JoystickTabletPlugin'>
	^ nil
