Extension { #name : 'ClassFactoryForTestCase' }

{ #category : '*Deprecated12' }
ClassFactoryForTestCase >> defaultCategory [

	self deprecated: 'Use #packageName and #tagName instead because the concept of categories is been removed from Pharo.'.
	^ (self packageName , '-' , self tagName) asSymbol
]

{ #category : '*Deprecated12' }
ClassFactoryForTestCase >> defaultTagPostfix [

	self deprecated: 'Use #tagName instead.' transformWith: '`@rcv defaultTagPostfix' -> '`@rcv tagName'.
	^ self tagName
]

{ #category : '*Deprecated12' }
ClassFactoryForTestCase >> deleteClass: aClass [

	self deprecated: 'Use #delete: instead. The behavior is the same.' transformWith: '`@rcv deleteClass: `@arg' -> '`@rcv -> delete: `@arg'.
	self delete: aClass
]

{ #category : '*Deprecated12' }
ClassFactoryForTestCase >> newClassInCategory: category [

	self deprecated: 'Use the method #make: with the right parameters instead. See ClassFactoryForTestCase>>#newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: for the API and #make: to find the default value I am setting.'.
	^ self make: [ :aBuilder | aBuilder tag: category ]
]

{ #category : '*Deprecated12' }
ClassFactoryForTestCase >> newClassUsing: aTraitComposition [

	self deprecated: 'Use the method #make: with the right parameters instead. See ClassFactoryForTestCase>>#newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: for the API and #make: to find the default value I am setting.'.
	^ self make: [ :aBuilder | aBuilder traitComposition: aTraitComposition ]
]

{ #category : '*Deprecated12' }
ClassFactoryForTestCase >> newClassWithInstanceVariableNames: aString [

	self deprecated: 'Use the method #make: with the right parameters instead. See ClassFactoryForTestCase>>#newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: for the API and #make: to find the default value I am setting.'.
	^ self make: [ :aBuilder | aBuilder slotsFromString: aString ]
]

{ #category : '*Deprecated12' }
ClassFactoryForTestCase >> newSubclassOf: aClass [

	self deprecated: 'Use the method #make: with the right parameters instead. See ClassFactoryForTestCase>>#newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: for the API and #make: to find the default value I am setting.'.
	^ self make: [ :aBuilder | aBuilder superclass: aClass ]
]

{ #category : '*Deprecated12' }
ClassFactoryForTestCase >> newSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames: classVarsString [

	self deprecated: 'Use the method #make: with the right parameters instead. See ClassFactoryForTestCase>>#newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: for the API and #make: to find the default value I am setting.'.
	^ self make: [ :aBuilder |
		  aBuilder
			  superclass: aClass;
			  slotsFromString: ivNamesString;
			  sharedVariablesFromString: classVarsString ]
]

{ #category : '*Deprecated12' }
ClassFactoryForTestCase >> newSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames: classVarsString category: category [

	self deprecated: 'Use the method #make: with the right parameters instead. See ClassFactoryForTestCase>>#newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: for the API and #make: to find the default value I am setting.'.
	^ self make: [ :aBuilder |
		  aBuilder
			  superclass: aClass;
			  slotsFromString: ivNamesString;
			  sharedVariablesFromString: classVarsString;
			  tag: category ]
]

{ #category : '*Deprecated12' }
ClassFactoryForTestCase >> newSubclassOf: aClass uses: aTraitComposition instanceVariableNames: ivNamesString classVariableNames: classVarsString category: category [

	self deprecated: 'Use the method #make: with the right parameters instead. See ClassFactoryForTestCase>>#newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: for the API and #make: to find the default value I am setting.'.
	^ self make: [ :aBuilder |
		  aBuilder
			  superclass: aClass;
			  traitComposition: aTraitComposition;
			  slotsFromString: ivNamesString;
			  sharedVariablesFromString: classVarsString;
			  package: self packageName;
			  tag: category ]
]

{ #category : '*Deprecated12' }
ClassFactoryForTestCase >> newSubclassOf: aClass uses: aTraitComposition instanceVariableNames: ivNamesString classVariableNames: classVarsString poolDictionaries: poolNamesString category: category [

	self deprecated: 'Use the method #make: with the right parameters instead. See ClassFactoryForTestCase>>#newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: for the API and #make: to find the default value I am setting.'.
	^ self make: [ :aBuilder |
		  aBuilder
			  superclass: aClass;
			  traitComposition: aTraitComposition;
			  slotsFromString: ivNamesString;
			  sharedVariablesFromString: classVarsString;
			  sharedPoolsFromString: poolNamesString;
			  package: self packageName;
			  tag: category ]
]

{ #category : '*Deprecated12' }
ClassFactoryForTestCase >> newSubclassOf: aClass using: aTraitComposition [

	self deprecated: 'Use the method #make: with the right parameters instead. See ClassFactoryForTestCase>>#newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: for the API and #make: to find the default value I am setting.'.
	^ self make: [ :aBuilder |
		  aBuilder
			  superclass: aClass;
			  traitComposition: aTraitComposition ]
]

{ #category : '*Deprecated12' }
ClassFactoryForTestCase >> newTraitInTag: aTag [

	self deprecated: 'Use the method #make: with the right parameters instead. See ClassFactoryForTestCase>>#newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: for the API and #make: to find the default value I am setting.'.
	^ self make: [ :aBuilder |
		  aBuilder
			  beTrait;
			  tag: aTag ]
]

{ #category : '*Deprecated12' }
ClassFactoryForTestCase >> newTraitNamed: aTraitName uses: aTraitComposition tag: aTag [

	self deprecated: 'Use the method #make: with the right parameters instead. See ClassFactoryForTestCase>>#newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: for the API and #make: to find the default value I am setting.'.
	^ self make: [ :aBuilder |
		  aBuilder
			  name: aTraitName;
			  traitComposition: aTraitComposition;
			  tag: aTag;
			  beTrait ]
]

{ #category : '*Deprecated12' }
ClassFactoryForTestCase >> newTraitUsing: aTraitComposition [

	self deprecated: 'Use the method #make: with the right parameters instead. See ClassFactoryForTestCase>>#newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: for the API and #make: to find the default value I am setting.'.
	^ self make: [ :aBuilder |
		  aBuilder
			  beTrait;
			  traitComposition: aTraitComposition ]
]

{ #category : '*Deprecated12' }
ClassFactoryForTestCase >> redefineClass: aClass category: aString [

	self deprecated: 'Use the method #update:to: instead'.
	^ self update: aClass to: [ :aBuilder |
		  aBuilder package: aString ]
]

{ #category : '*Deprecated12' }
ClassFactoryForTestCase >> redefineClass: aClass instanceVariableNames: aString [

	self deprecated: 'Use the method #update:to: instead'.
	^ self update: aClass to:[ :aBuilder |
		  aBuilder slotsFromString: aString ]
]

{ #category : '*Deprecated12' }
ClassFactoryForTestCase >> redefineClass: aClass subclassOf: aSuperclass uses: aTraitComposition instanceVariableNames: ivNamesString classVariableNames: classVarsString poolDictionaries: poolNamesString package: aPackageName [

	self deprecated: 'Use the method #make: with the right parameters instead. See ClassFactoryForTestCase>>#newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: for the API and #make: to find the default value I am setting.'.
	^ self make: [ :aBuilder |
		  aBuilder
			  name: aClass name;
			  superclass: aSuperclass;
			  traitComposition: aTraitComposition;
			  slotsFromString: ivNamesString;
			  sharedVariablesFromString: classVarsString;
			  sharedPoolsFromString: poolNamesString;
			  package: aPackageName asSymbol ]
]

{ #category : '*Deprecated12' }
ClassFactoryForTestCase >> silentlyNewClassInCategory: category [

	self deprecated: 'Use the method #make: with the right parameters instead. See ClassFactoryForTestCase>>#newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: for the API and #silentlyMake: to find the default value I am setting.'.
	^ self silentlyMake: [ :aBuilder | aBuilder tag: category ]
]

{ #category : '*Deprecated12' }
ClassFactoryForTestCase >> silentlyNewSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames: classVarsString [

	self deprecated:
		'Use the method #make: with the right parameters instead. See ClassFactoryForTestCase>>#newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: for the API and #silentlyMake: to find the default value I am setting.'.
	^ self silentlyMake: [ :aBuilder |
		  aBuilder
			  superclass: aClass;
			  slotsFromString: ivNamesString;
			  sharedVariablesFromString: classVarsString;
			  tag: self tagName ]
]

{ #category : '*Deprecated12' }
ClassFactoryForTestCase >> silentlyNewSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames: classVarsString category: category [
	
	self deprecated: 'Use the method #make: with the right parameters instead. See ClassFactoryForTestCase>>#newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: for the API and #silentlyMake: to find the default value I am setting.'.
	^ self silentlyMake: [ :aBuilder |
		  aBuilder
			  superclass: aClass;
			  slotsFromString: ivNamesString;
			  sharedVariablesFromString: classVarsString;
			  tag: category ]
]

{ #category : '*Deprecated12' }
ClassFactoryForTestCase >> silentlyNewSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames: classVarsString package: packageName [

	self deprecated: 'Use the method #make: with the right parameters instead. See ClassFactoryForTestCase>>#newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: for the API and #silentlyMake: to find the default value I am setting.'.
	^ self silentlyMake: [ :aBuilder |
		  aBuilder
			  superclass: aClass;
			  slotsFromString: ivNamesString;
			  sharedVariablesFromString: classVarsString;
			  package: self packageName ]
]
