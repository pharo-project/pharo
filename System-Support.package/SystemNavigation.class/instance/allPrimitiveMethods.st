query
allPrimitiveMethods
	"Answer an OrderedCollection of all the methods that are implemented by primitives."
	| aColl |
	aColl := OrderedCollection new: 200.
	self allBehaviorsDo: [:class | class
		selectorsAndMethodsDo: [:sel :method | 
			method isPrimitive
				ifTrue: [aColl addLast: class name , ' ' , sel , ' ' , method primitive printString]]].
	^ aColl