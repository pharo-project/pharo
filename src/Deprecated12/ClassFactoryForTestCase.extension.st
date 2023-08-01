Extension { #name : #ClassFactoryForTestCase }

{ #category : #'*Deprecated12' }
ClassFactoryForTestCase >> newClassInCategory: category [

	self deprecated: 'Use the method #make: with the right parameters instead. See ClassFactoryForTestCase>>#newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: for the API and #make: to find the default value I am setting.'.
	^ self make: [ :aBuilder | aBuilder tag: category ]
]

{ #category : #'*Deprecated12' }
ClassFactoryForTestCase >> newClassUsing: aTraitComposition [

	self deprecated: 'Use the method #make: with the right parameters instead. See ClassFactoryForTestCase>>#newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: for the API and #make: to find the default value I am setting.'.
	^ self make: [ :aBuilder | aBuilder traitComposition: aTraitComposition ]
]
