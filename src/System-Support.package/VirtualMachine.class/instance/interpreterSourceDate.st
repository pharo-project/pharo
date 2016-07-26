accessing
interpreterSourceDate
	"Return the date of changes given by `self interpreterSourceVersion`"
	| dateString parts | 
	
	dateString := (self interpreterSourceVersion splitOn: 'Date: ') second.
	
	dateString first isDigit
		ifTrue: [ 
			"most probably the date is in ISO 8601 Format" 
			^ dateString asDateAndTime ].
		
	"Otherwise assume the old format: DDD MMM DD HH:MM:SS YYYY +TTTT ..."
	parts := dateString substrings first: 6.
	"create a more reasonable string.."
	dateString := String streamContents: [ :s |
		s 
			nextPutAll: (parts at: 5 ); space; "year"
			nextPutAll: (parts at: 2 ); space; "month name"
			nextPutAll: (parts at: 3 ); space; "day of month"
			nextPutAll: (parts at: 4 ); space; "time"
			nextPutAll: (parts last); space    "timezone"].
	^ dateString asDateAndTime 