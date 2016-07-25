A CalendarDyaMorph represents a specific day on a monthly calendar.


Instance Variables
	bounds:		<Rectangle>
	date:		<Date>
	highlighted:	<Boolean>
	owner:		<CalendarChooserMorph>

bounds
	- owner-relative bounding box

date
	- the specific date (year/month/day) the CalendarMorph represents

highlighted
	- flag to keep track of when a CalendarMorph has the mouse dragging over it, and is thus highlighted

owner
	- the morph that contains the CalendarMorph, and all its siblings
