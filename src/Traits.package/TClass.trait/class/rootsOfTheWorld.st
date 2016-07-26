inquiries
rootsOfTheWorld
	"return all classes that have a nil superclass"
	
	^(self environment select: [:each | each isBehavior and: [each superclass isNil]]) asOrderedCollection