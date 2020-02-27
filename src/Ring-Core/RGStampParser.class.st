Class {
	#name : #RGStampParser,
	#superclass : #Object,
	#category : #'Ring-Core-Parsing'
}

{ #category : #parsing }
RGStampParser class >> authorForStamp: aString [
	
	^ aString isEmptyOrNil
		ifTrue: [ self historicalStamp ]
		ifFalse: [ self parseAuthorAliasFrom: aString ].
]

{ #category : #parsing }
RGStampParser class >> historicalStamp [

	^ '<historical>'
]

{ #category : #parsing }
RGStampParser class >> parseAuthorAliasFrom: aString [
		
	^ self new basicParseAuthorAliasFrom: aString
]

{ #category : #parsing }
RGStampParser class >> parseTimestampFrom: aString [
		
	^ self new parseTimestampFrom: aString
]

{ #category : #parsing }
RGStampParser class >> timeForStamp: stamp [

	^ self new timeForStamp: stamp
]

{ #category : #'parsing stamp' }
RGStampParser >> basicParseAuthorAliasFrom: aString [
	"Parse an alias/name of the author from a string that is extracted from a source file. If there is no alias/name we return emtpy string."

	| tokens dateStartIndex unknown |
	"The following timestamp strings are supported (source: squeak sources archeological survey):
		<authorname><date><time>. 
		<authorname><date>
		<date><time>
		<date><time><authorname>
		<date><authorname>
		<historical>
	All fields can be separated by spaces or line ends but a separator between author alias/name and date can be missing as well"
	"unknown:= 'unknown'."
	
	aString = '<historical>' ifTrue: [ ^ aString ].
	
	unknown := nil.
	aString isEmptyOrNil
		ifTrue: [ ^ unknown ].
	dateStartIndex := (aString indexOf: $/) - 1.	"If there is no / character in the timestamp, no author alias/name exists"
	dateStartIndex = -1
		ifTrue: [ ^ unknown ].
	^ [ 
	"Go the start of the date string (there can be 1 or 2 digits and a space separator can be missing at the front!!)"
	(dateStartIndex >= 2 and: [ (aString at: dateStartIndex - 1) isDigit ])
		ifTrue: [ dateStartIndex := dateStartIndex - 1 ].	"Extract only those tokens that do not possible represent date or time - meaning that authorname may be at the end"
	tokens := (aString copyFrom: dateStartIndex to: aString size) substrings
		reject: [ :token | (token occurrencesOf: $/) = 2 or: [ (token occurrencesOf: $:) = 1 ] ].	"only one token should be left if author name/alias exists"
	^ tokens isEmpty
		ifTrue: [ 
			"if dateStartIndex is not 1 then the authorname may be at the beginning"
			dateStartIndex > 1
				ifTrue: [ (aString copyFrom: 1 to: dateStartIndex - 1) trimBoth ]
				ifFalse: [ unknown ] ]
		ifFalse: [ tokens first ] ]
		on: Exception
		do: [ :e | unknown ]
]

{ #category : #'parsing stamp' }
RGStampParser >> parseAuthorAliasFrom: aString [
	"Allows other applications  to treat a different empty alias by overriding this method"
	
	^self basicParseAuthorAliasFrom: aString
]

{ #category : #parsing }
RGStampParser >> parseTimestampFrom: aString [
	
	^self parseTimestampFrom: aString default: nil
]

{ #category : #parsing }
RGStampParser >> parseTimestampFrom: aString default: anObject [
	"Parse a date-time from a timestamp-string that is extracted from a source file. If there is no timestamp, or we cannot make sense of it, we return the default value."

	| tokens dateStartIndex unknown |
	"The following timestamp strings are supported (source: squeak sources archeological survey):
		<authorname><date><time>. 
		<authorname><date>
		<date><time>
		<date><time><authorname>
		<date><authorname>
		<historical>
	All fields can be separated by spaces or line ends but a separator between authorname and date can be missing as well"
	unknown := anObject.
	aString isEmptyOrNil
		ifTrue: [ ^ unknown ].
	dateStartIndex := (aString indexOf: $/) - 1.	"If there is no / character in the timestamp, we cannot parse a date and return the epoch"
	dateStartIndex = -1
		ifTrue: [ ^ unknown ].
	^ [ 
	"Go the start of the date string (there can be 1 or 2 digits and a space separator can be missing at the front!!)"
	(dateStartIndex >= 2 and: [ (aString at: dateStartIndex - 1) isDigit ])
		ifTrue: [ dateStartIndex := dateStartIndex - 1 ].	"Extract only those tokens that possibly represent date or time"
	tokens := (aString copyFrom: dateStartIndex to: aString size) substrings
		select: [ :token | (token occurrencesOf: $/) = 2 or: [ (token occurrencesOf: $:) = 1 ] ].	"2 tokens is a datetime"
	tokens size = 2
		ifTrue: [ (tokens joinUsing: Character space) asDateAndTime ]
		ifFalse: [ tokens first asDate asDateAndTime ] ]
		on: Exception
		do: [ :e | unknown ]
]

{ #category : #'parsing stamp' }
RGStampParser >> timeForStamp: stamp [

	^ stamp
		ifNil: [ 	DateAndTime new]
		ifNotNil: [ 
			| substrings |
			substrings := stamp substrings.
			([(DateAndTime fromString: (' ' join: substrings allButFirst ))] on: Exception do: [DateAndTime epoch]) ]. 
	
]
