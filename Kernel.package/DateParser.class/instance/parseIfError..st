parsing
parseIfError: aBlock
	"Parse the date according to the given pattern"
	self initializeParsing.
	
	[ self isDoneParsing ] whileFalse: [ self parseNextPattern ].

	self isInvalidPattern ifTrue: aBlock.
	self convertTwoDigitsYear.
	
	(inputStream atEnd and: [ patternStream atEnd ]) ifFalse: [ DateError signal: 'Input doesn''t match given pattern.' ].
	
	^ self createDate