"
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

"
Class {
	#name : #CalendarDayMorph,
	#superclass : #Object,
	#instVars : [
		'date',
		'bounds',
		'owner',
		'highlighted'
	],
	#category : #'Morphic-Widgets-Extra'
}

{ #category : #'instance creation' }
CalendarDayMorph class >> on: aDate for: aCalendarChooserMorph [

	^self new
		date: aDate;
		owner: aCalendarChooserMorph;
		yourself.

]

{ #category : #accessing }
CalendarDayMorph >> bounds [

	^ bounds
]

{ #category : #accessing }
CalendarDayMorph >> bounds: aRectangle [

	bounds := aRectangle
]

{ #category : #accessing }
CalendarDayMorph >> date [

	^ date
]

{ #category : #accessing }
CalendarDayMorph >> date: aDate [

	date := aDate.

]

{ #category : #printing }
CalendarDayMorph >> debugPrint [

	^(WriteStream on: (String new: 10))
		print: self class;
		nextPutAll: ' (';
		print: self date;
		nextPutAll: ' - ';
		print: self bounds;
		nextPut: $);
		contents
]

{ #category : #drawing }
CalendarDayMorph >> drawOn: aCanvas offset: origin [

	| box dayString textColor textTopLeft textWidth today |
	dayString := date dayOfMonth printString.
	textWidth := owner weekdayFont widthOfString: dayString.
	textTopLeft := bounds topCenter translateBy: (textWidth // -2) @ 3.
	box := ((textTopLeft extent: textWidth @ owner weekdayFont height) insetBy: -8 @ -1) translateBy: origin.
	today := date = Date today.
	textColor := date month = owner date month
		ifTrue: [Color black]
		ifFalse: [Color veryLightGray].
	(date = owner date or: [self highlighted])
		ifTrue: [
			| lineColor fillColor |
			lineColor := today
				ifTrue: [Color red]
				ifFalse: [Color veryLightGray].
			fillColor := self highlighted
				ifTrue: [Color veryVeryLightGray]
				ifFalse: [Color veryLightGray].
			aCanvas fillOval: box color: fillColor borderWidth: 1 borderColor: lineColor].
	today & (date ~= owner date) & self highlighted not
		ifTrue: [aCanvas fillOval: box color: Color white borderWidth: 1 borderColor: Color red].
	aCanvas
		drawString: dayString
		at: textTopLeft + origin
		font: owner weekdayFont
		color: textColor.
]

{ #category : #accessing }
CalendarDayMorph >> highlighted [

	^highlighted
]

{ #category : #accessing }
CalendarDayMorph >> highlighted: aBoolean [

	highlighted := aBoolean
]

{ #category : #initialization }
CalendarDayMorph >> initialize [
	super initialize.
	self highlighted: false
]

{ #category : #accessing }
CalendarDayMorph >> owner [

	^ owner
]

{ #category : #accessing }
CalendarDayMorph >> owner: aCalendarChooserMorph [

	owner := aCalendarChooserMorph
]

{ #category : #printing }
CalendarDayMorph >> printOn: aStream [

	aStream
		print: self class;
		nextPutAll: ' (';
		print: self date;
		nextPut: $)
]
