private
valueAtNewKey: aKey put: anObject atIndex: index declareFrom: aDictionary 
	"Support for coordinating class variable and global declarations
	with variables that have been put in Undeclared so as to
	redirect all references to the undeclared variable."

	(aDictionary includesKey: aKey)
		ifTrue: 
			[self atNewIndex: index 
				put: ((aDictionary associationAt: aKey) value: anObject).
			aDictionary removeKey: aKey]
		ifFalse: 
			[self atNewIndex: index put: (Association key: aKey value: anObject)]