private
processLinkTo: newObject from: anObject 
	newObject class instSize + newObject basicSize = 0 
		ifTrue: 
			[^(testBlock value: newObject) 
				ifTrue: 
					[(self computePathFor: anObject)
						add: newObject;
						yourself]
				ifFalse: [nil]].
	(self hasSearched: newObject) 
		ifFalse: 
			[(testBlock value: newObject) 
				ifTrue: 
					[^(self computePathFor: anObject)
						add: newObject;
						yourself].
			objectsLeft add: newObject.
			self addBacklinkFrom: newObject to: anObject].
	^nil