query
allClassesImplementing: aSelector  
	"Answer an Array of all classes that implement the message aSelector."

	| aCollection |
	aCollection := ReadWriteStream on: Array new.
	self allBehaviorsDo: [:class | (class includesSelector: aSelector) ifTrue: [aCollection nextPut: class]].
	^ aCollection contents