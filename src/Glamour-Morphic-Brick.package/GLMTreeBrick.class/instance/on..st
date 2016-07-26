instance creation
on: anObject
	"{ 'first'-> { 'sub 1' -> '11' . 'sub 2' -> '12' } }"
	
	self removeAllBricks.
	(anObject isCollection & anObject isString not)
		ifFalse: [ self addBrickBack: (self collapsableFor: anObject) ]
		ifTrue: [ anObject do: [ :each | self addBrickBack: (self collapsableFor: each) ] ].
	
	