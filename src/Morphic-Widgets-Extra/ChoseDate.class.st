"
A ChoseDate is an announcement raised by the CalendarMorph
"
Class {
	#name : #ChoseDate,
	#superclass : #Announcement,
	#instVars : [
		'date',
		'calendar'
	],
	#category : #'Morphic-Widgets-Extra'
}

{ #category : #'instance creation' }
ChoseDate class >> of: aDate from: aCalendarChooserMorph [

	^ self new
		date: aDate;
		calendar: aCalendarChooserMorph.
]

{ #category : #accessing }
ChoseDate >> calendar [

	^ calendar.
]

{ #category : #private }
ChoseDate >> calendar: aCalendarChooserMorph [ 

	calendar := aCalendarChooserMorph.
]

{ #category : #accessing }
ChoseDate >> date [

	^ date.
]

{ #category : #private }
ChoseDate >> date: aDate [ 

	date := aDate.
]
