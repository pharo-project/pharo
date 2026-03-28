Extension { #name : 'String' }

{ #category : '*System-Time' }
String >> asDateAndTime [
	"Convert from UTC format"

	^ DateAndTime fromString: self
]

{ #category : '*System-Time' }
String >> asDuration [
	"Convert from [-]D:HH:MM:SS[.S] format. What is between [] implies optional elements"

	^ Duration fromString: self
]

{ #category : '*System-Time' }
String >> asTime [
	"Many allowed forms, see Time>>readFrom:"
	self
		deprecated: 'Use Time fromString: instead'
		on: '27/03/2025'
		in: 'Pharo14'
		transformWith: '`@receiver asTime' -> 'Time fromString: `@receiver'.
	^ Time fromString: self
]