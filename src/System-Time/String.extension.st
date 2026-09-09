Extension { #name : 'String' }

{ #category : '*System-Time' }
String >> asDateAndTime [
 	"Convert from UTC format"
	
	"self deprecated: 'Use DateAndTime fromString: ' transformWith: '`@rec asDateAndTime' -> 'DateAndTime fromString: `@rec'.
	"
	self  deprecated:  'Use DateAndTime fromString: instead'.
	^ DateAndTime fromString: self
]

{ #category : '*System-Time' }
String >> asDuration [
	"Convert from [-]D:HH:MM:SS[.S] format. What is between [] implies optional elements"

	self deprecated: 'Rewrite `aString asDuration` as `Duration fromString: aString`'.
	^ Duration fromString: self
]

{ #category : '*System-Time' }
String >> asTime [
	"Many allowed forms, see Time>>readFrom:"

	^ Time fromString: self
]
