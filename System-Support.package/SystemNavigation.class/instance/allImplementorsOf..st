query
allImplementorsOf: aSelector 
	"Answer all the methods that implement the message aSelector."
	
	| aCollection |
	aCollection := OrderedCollection  new.
	self allBehaviorsDo: [:class | 
		(class includesSelector: aSelector)
			ifTrue: [aCollection add: (self createMethodNamed: aSelector realParent: class)]].
	^ aCollection