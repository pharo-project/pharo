accessing
partNamed: fieldName ifNone: block
	parts isNil ifTrue: [ block value ].
	^ self parts detect: [ :each | each fieldName = fieldName ] ifNone: block