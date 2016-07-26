generating
generateViolations: aCollection source: aString offset: aPoint on: aStream
	aCollection do: [ :rule |
		| interval start |
		interval := (rule result selectionIntervalFor: aString)
			ifNil: [ 1 to: aString size ].
		start := self lineAndColumn: aString at: interval first.
		aStream tab; tab; nextPutAll: '<error'.
		aStream nextPutAll: ' source="'; nextPutAll: (self encode: rule class name); nextPut: $".
		aStream nextPutAll: ' message="'; nextPutAll: (self encode: rule group); nextPutAll: ' - '; nextPutAll: (self encode: rule name); nextPut: $".
		aStream nextPutAll: ' severity="'; nextPutAll: (rule
			severity = #information 
				ifTrue: [ 'info' ]
				ifFalse: [ 
					rule severity = #warning
						ifTrue: [ 'warning' ]
						ifFalse: [ 'error' ] ]); nextPut: $".
		aStream nextPutAll: ' line="'; print: aPoint x + start x; nextPut: $".
		aPoint y + start y - 1 <= 1
			ifFalse: [ aStream nextPutAll: ' column="'; print: aPoint y + start y - 1; nextPut: $" ].
		aStream nextPutAll: '/>'; nextPut: Character lf ]