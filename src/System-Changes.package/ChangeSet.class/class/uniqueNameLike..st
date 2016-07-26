defaults
uniqueNameLike: aString 
	| try index |
	(self named: aString)
		ifNil: [ ^ aString ].
	index := 1.
	[ try := aString , index printString.
	  (self named: try)
		ifNil: [ ^ try ].
	  index := index + 1 ] repeat
