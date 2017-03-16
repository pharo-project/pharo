private
timeMorphs
	
	^ self timeString
		ifEmpty: [ #() ]
		ifNotEmpty: [:aString | { self morphFactory secondaryStringFor: aString } ]