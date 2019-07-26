"
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

"
Class {
	#name : #CalendarMorph,
	#superclass : #BorderedMorph,
	#instVars : [
		'date',
		'days',
		'touchPoints',
		'announcer'
	],
	#category : #'Morphic-Widgets-Extra'
}

{ #category : #'instance creation' }
CalendarMorph class >> on: aDate [

	^self new
		"extent: 200 @ 160;"
		date: aDate;
		yourself.

]

{ #category : #'instance creation' }
CalendarMorph class >> openOn: aDate [

	^(self on: aDate) openInWorld
]

{ #category : #private }
CalendarMorph >> announceDate [

	| announcement |
	announcement := ChoseDate of: self date from: self.
	announcer ifNotNil: [ announcer announce: announcement ].
]

{ #category : #private }
CalendarMorph >> announcer [

	^ announcer ifNil: [ announcer := Announcer new ].
]

{ #category : #private }
CalendarMorph >> computeDays [
	"Populate the days instance variable with CalendarChooserDay instances for the receiver's month."

	| cellHeight cellWidth height topLeft lastMonth nextMonth theDay thisMonth |
	topLeft := 0 @ 25.
	height := self bounds height - 25.
	cellHeight := height // 8.
	height := height - cellHeight.
	cellWidth := self bounds width // 7.
	lastMonth := date month previous.
	thisMonth := date month.
	nextMonth := date month next.
	days := OrderedCollection new.
	1 to: 6 do: [:lineIndex |
		| yOffset |
		yOffset := topLeft y + (lineIndex * cellHeight).
		(self daysForLine: lineIndex) withIndexDo: [:day :dayIndex |
			| cellPosX dayDate |
			dayDate := thisMonth asDate addDays: day - 1.
			(lineIndex = 1 and: [day > 7])
				ifTrue: [dayDate := lastMonth asDate addDays: day - 1].
			(lineIndex > 4 and: [day < 15])
				ifTrue: [dayDate := nextMonth asDate addDays: day - 1].
			cellPosX := cellWidth * (dayIndex - 1).
			days add: (theDay := CalendarDayMorph on: dayDate for: self).
			theDay bounds: (cellPosX @ yOffset extent: cellWidth @ cellHeight)]]
]

{ #category : #accessing }
CalendarMorph >> date [

	^date
]

{ #category : #accessing }
CalendarMorph >> date: aDate [

	| recompute |
	recompute := date isNil or: [date month ~= aDate month].
	date := aDate.
	recompute
		ifTrue: [self computeDays]
]

{ #category : #private }
CalendarMorph >> daysForLine: aNumber [
	"Return an array of numbers that correspond to the day-of-month numbers of the given line (row) 
	in the calendar for the month of the receiver's date."

	| dayCount firstWeekday previousDayCount previousMonthDays lastDay |
	dayCount := date month daysInMonth.
	firstWeekday := Date firstWeekdayOfMonth: date monthIndex year: date year.
	previousDayCount := date month previous daysInMonth.
	"First case - handle the first line specially"
	aNumber = 1
		ifTrue: [
			"If this month's first day is Sunday, the first line is the last week from last month"
			firstWeekday = 1
				ifTrue: [^(previousDayCount - 6 to: previousDayCount) asArray].

			"Otherwise, its a mix of last month and this month"
			previousMonthDays := (firstWeekday - 1 to: 1 by: -1) collect: [:each | previousDayCount - each + 1].
			^previousMonthDays, ((1 to: 7) asArray copyFrom: 1 to: 7 - previousMonthDays size)].

	"Recompute the last day from the previous line (I love recursion)"
	lastDay := (self daysForLine: aNumber - 1) last.
	"Second case - the first week of this month starts on Sunday"
	(aNumber = 2 and: [lastDay = previousDayCount])
		ifTrue: [^(1 to: 7) asArray].

	"Third case - the first week of next month starts on Sunday"
	lastDay = dayCount
		ifTrue: [^(1 to: 7) asArray].

	"Fourth case - everything else"
	^(lastDay + 1 to: lastDay + 7) collect: [:each |
		each <= dayCount
			ifTrue: [each]
			ifFalse: [each - dayCount]]
]

{ #category : #initialize }
CalendarMorph >> defaultBounds [
	"Answer the default bounds for the receiver."

	^0 @ 0 corner: 200 @ 160
]

{ #category : #drawing }
CalendarMorph >> drawDaysOn: aCanvas [

	days do: [:each |
		each
			drawOn: aCanvas
			offset: self bounds topLeft]
]

{ #category : #drawing }
CalendarMorph >> drawMonthHeaderOn: aCanvas [

	| headerWidth headerString box textBox textTopLeft monthBox monthNameWidth yearBox previousMonthBox nextMonthBox |
	headerString := date asMonth printString.
	headerWidth := self monthNameFont widthOfString: headerString.
	box := self bounds topLeft extent: self bounds width @ 23.
	textTopLeft := self bounds topCenter translateBy: (headerWidth // -2) @ 5.
	textBox := textTopLeft extent: headerWidth @ self monthNameFont height.
	monthNameWidth := self monthNameFont widthOfString: self date monthName, ' '.
	monthBox := textBox topLeft extent: monthNameWidth @ textBox height.
	yearBox := monthBox topRight corner: textBox bottomRight.
	previousMonthBox := (self bounds topLeft translateBy: 10 @ 5) extent: 10 @ self monthNameFont height.
	nextMonthBox := (self bounds topRight translateBy: -20 @ 5) extent: 10 @ self monthNameFont height.
	touchPoints
		at: monthBox put: #handleMonthNameTouched;
		at: yearBox put: #handleYearTouched;
		at: (previousMonthBox expandBy: 10 @ 5) put: #handlePreviousMonthTouched;
		at: (nextMonthBox expandBy: 10 @ 5) put: #handleNextMonthTouched.
	aCanvas
		frameAndFillRectangle: box
		fillColor: Color veryLightGray
		borderWidth: 1
		borderColor: Color black;
		
		line: box bottomLeft
		to: box bottomRight
		width: 2
		color: Color black;
		
		drawString: '<'
		at: previousMonthBox origin
		font: self monthNameFont
		color: Color black;
		
		drawString: '>'
		at: nextMonthBox origin
		font: self monthNameFont
		color: Color black;

		drawString: headerString
		at: (self bounds topCenter translateBy: (headerWidth // -2) @ 5)
		font: self monthNameFont
		color: Color black.

]

{ #category : #drawing }
CalendarMorph >> drawOn: aCanvas [

	touchPoints := Dictionary new.
	aCanvas
		clipBy: self bounds
		during: [:clippedCanvas |
			clippedCanvas
				fillRectangle: self bounds
				color: Color white.

			self 
				drawMonthHeaderOn: clippedCanvas;
				drawWeekDayNamesOn: clippedCanvas;
				drawDaysOn: clippedCanvas;
				drawTodayOn: aCanvas.

			clippedCanvas
				frameRectangle: self bounds
				width: 1
				color: Color black].



	
]

{ #category : #drawing }
CalendarMorph >> drawTodayOn: aCanvas [

	| text textHeight textTopLeft textWidth textBox |
	text := 'Today: ', (Date today printFormat: #(2 1 3 $  3 1 1)).
	textWidth := self weekdayFont widthOfString: text.
	textHeight := self weekdayFont height.
	textTopLeft := self bounds bottomCenter translateBy: (textWidth // -2) @ (textHeight negated - 5).
	textBox := textTopLeft extent: textWidth @ textHeight.
	touchPoints at: textBox put: #handleTodayTouched.
	aCanvas
		drawString: text
		at: textTopLeft
		font: self weekdayFont
		color: Color gray
]

{ #category : #drawing }
CalendarMorph >> drawWeekDayNamesOn: aCanvas [

	| cellHeight height topLeft topRight cellWidth |
	topLeft := self bounds topLeft translateBy: 0 @ 25.
	topRight := self bounds topRight translateBy: 0 @ 25.
	height := self bounds height - 25.
	cellHeight := height // 8.
	cellWidth := self bounds width // 7.
	aCanvas
		line: (topLeft translateBy: 0 @ cellHeight) 
		to: (topRight translateBy: 0 @ cellHeight) 
		width: 1
		color: Color black.

	#('Sun' 'Mon' 'Tue' 'Wed' 'Thu' 'Fri' 'Sat') withIndexDo: [:dayName :dayIndex |
		| cellPosX cellTopCenter textWidth |
		cellPosX := cellWidth * (dayIndex - 1).
		cellTopCenter := topLeft translateBy: ((cellPosX + (cellWidth // 2)) + 1) @ 0.
		textWidth := self weekdayFont widthOfString: dayName.
		aCanvas
			drawString: dayName
			at: (cellTopCenter translateBy: (textWidth // -2) @ 3)
			font: self weekdayFont
			color: Color darkGray]

]

{ #category : #accessing }
CalendarMorph >> extent: aPoint [
	"Since the day objects cache their bounding box, we have to recompute them if the receiver resizes."

	| result |
	result := super extent: aPoint.
	date notNil
		ifTrue: [self computeDays].
	^result
]

{ #category : #'event handling' }
CalendarMorph >> handleMonthNameTouched [

	| newMonthName dayCount |

	newMonthName := UIManager default
		chooseDropList: 'Choose a month:'
		list:
			#('January' 'February' 'March' 'April' 'May' 'June' 'July' 'August' 'September' 'October' 'November' 'December').

	newMonthName ifNil: [ ^ self ].

	dayCount := ( Month year: date year month: newMonthName ) daysInMonth.
	self date: ( Date year: date year month: newMonthName day: ( date dayOfMonth min: dayCount ) ).
	self changed
]

{ #category : #'event handling' }
CalendarMorph >> handleNextMonthTouched [

	self date: date onNextMonth.
	self changed.

]

{ #category : #'event handling' }
CalendarMorph >> handlePreviousMonthTouched [

	self date: date onPreviousMonth.
	self changed.

]

{ #category : #'event handling' }
CalendarMorph >> handleTodayTouched [

	self date: Date today.
	self changed.

]

{ #category : #'event handling' }
CalendarMorph >> handleYearTouched [

	| newYear dayCount |

	newYear := UIManager default
		chooseOrRequestFrom: ( 2000 to: 2020 )
		lines: #()
		title: 'Choose a year:'.

	newYear ifNil: [ ^ self ].

	newYear := newYear asNumber.
	dayCount := ( Month year: newYear month: date monthIndex ) daysInMonth.
	self date: ( Date year: newYear month: date monthIndex day: ( date dayOfMonth min: dayCount ) ).
	self changed
]

{ #category : #'event handling' }
CalendarMorph >> handlesMouseDown: event [

	^true
]

{ #category : #initialization }
CalendarMorph >> initialize [

	super initialize.
	touchPoints := Dictionary new.

]

{ #category : #accessing }
CalendarMorph >> monthNameFont [

	| font boldItalic |
	font := LogicalFont familyName: 'Bitmap DejaVu Sans' pointSize: 12.
	boldItalic := TextEmphasis italic emphasisCode | TextEmphasis bold emphasisCode.
	^ font emphasis: boldItalic.
]

{ #category : #'event handling' }
CalendarMorph >> mouseDown: event [
	"Handle mouse down and mouse movement. Highlight the day under the mouse."

	| morphRelativeHitPoint |
	morphRelativeHitPoint := event cursorPoint translateBy: bounds origin negated.
	days do: [:each |
		each highlighted: (each bounds containsPoint: morphRelativeHitPoint)].
	self changed.

]

{ #category : #'event handling' }
CalendarMorph >> mouseMove: event [

	self mouseDown: event
]

{ #category : #'event handling' }
CalendarMorph >> mouseUp: event [
	"Check for hotspot hits - handle them if they match.
	Otherwise, convert the event cursor to morph-local, and find the day under it.
	If there is nothing under the mouse when it goes up, nothing happens."

	| morphRelativeHitPoint |
	touchPoints keysAndValuesDo: [:eachBox :eachSelector |
		(eachBox containsPoint: event cursorPoint)
			ifTrue: [self perform: eachSelector]].
	morphRelativeHitPoint := event cursorPoint translateBy: bounds origin negated.
	days do: [:each |
		each highlighted: false.
		(each bounds containsPoint: morphRelativeHitPoint)
			ifTrue: [
				self date: each date.
				self announceDate ]].
	self changed.

]

{ #category : #announcing }
CalendarMorph >> onChoiceSend: aSymbol to: anObject [

	self announcer when: ChoseDate send: aSymbol to: anObject.
]

{ #category : #accessing }
CalendarMorph >> weekdayFont [

	^ LogicalFont familyName: 'Bitmap DejaVu Sans' pointSize: 9.
]
