private
modify: aString for: aStringCollection

	aStringCollection size = 1 ifTrue: [ 
		^ aString surroundedBySingleQuotes ].
	aStringCollection first = aString ifTrue: [ 
		^ '''', aString, String space ].
	aStringCollection last = aString ifTrue: [ 
		^ String space, aString, '''' ].
	^ aString surroundedBy: String space.