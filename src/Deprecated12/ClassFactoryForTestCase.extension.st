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

{ #category : #'*Deprecated12' }
ClassFactoryForTestCase >> newClassWithInstanceVariableNames: aString [

	self deprecated: 'Use the method #make: with the right parameters instead. See ClassFactoryForTestCase>>#newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: for the API and #make: to find the default value I am setting.'.
	^ self make: [ :aBuilder | aBuilder slotsFromString: aString ]
]

{ #category : #'*Deprecated12' }
ClassFactoryForTestCase >> newSubclassOf: aClass [

	self deprecated: 'Use the method #make: with the right parameters instead. See ClassFactoryForTestCase>>#newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: for the API and #make: to find the default value I am setting.'.
	^ self make: [ :aBuilder | aBuilder superclass: aClass ]
]

{ #category : #'*Deprecated12' }
ClassFactoryForTestCase >> newSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames: classVarsString [

	self deprecated: 'Use the method #make: with the right parameters instead. See ClassFactoryForTestCase>>#newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: for the API and #make: to find the default value I am setting.'.
	^ self make: [ :aBuilder |
		  aBuilder
			  superclass: aClass;
			  slotsFromString: ivNamesString;
			  sharedVariablesFromString: classVarsString ]
]

{ #category : #'*Deprecated12' }
ClassFactoryForTestCase >> newSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames: classVarsString category: category [

	self deprecated: 'Use the method #make: with the right parameters instead. See ClassFactoryForTestCase>>#newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: for the API and #make: to find the default value I am setting.'.
	^ self make: [ :aBuilder |
		  aBuilder
			  superclass: aClass;
			  slotsFromString: ivNamesString;
			  sharedVariablesFromString: classVarsString;
			  tag: category ]
]
