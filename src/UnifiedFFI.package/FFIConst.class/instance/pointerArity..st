accessing
pointerArity: additionalArity
	additionalArity > 0 ifTrue: [ self error: 'passing pointer to constant' ]