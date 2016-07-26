activation
evaluateStdIn
	| stdin |
	stdin := FileStream stdin ifNil: [ ^ self ].
	stdin atEnd  ifTrue: [ ^ self ].
	
	self flag: #todo. "the whole compilation machinery does not work with stdin directly, hence we load everything a buffer first"
	self evaluate: (String streamContents: [ :s|
		[ stdin atEnd ] whileFalse: [ 
			stdin next ifNotNil: [ :char|
			s nextPut: char ]]])