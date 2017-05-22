accessing
value: aBoolean
	aBoolean 
		ifTrue: [ self addItem ]
		ifFalse: [ self removeItem ]
	