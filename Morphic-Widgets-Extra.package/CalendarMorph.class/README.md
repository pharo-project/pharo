A CalendarMorph is a standalone morph that represents a selectable monthly calendar.

CalendarMorph openOn: Date today



Instance Variables
	date:		<Date>
	days:		<OrderedCollection of: <CalendarChooserDay>>
	touchPoints:	<Dictionary key: <Rectangle> value: <Symbol>>

date
	- the currently selected date (always within the current month)

days
	- all the days that are visible, including days from the previous month, the current month, and the next month

touchPoints
	- extra hotspots that are touch-responsive (key rectangle is in world coordinates)
