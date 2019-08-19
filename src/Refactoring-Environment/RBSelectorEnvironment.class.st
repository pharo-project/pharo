"
I am a RBBrowserEnvironment for a set of selectors. 
Usually I am constructed as a result of a query on another environment:
env referencesTo:#aselector -> a RBSelectorEnvironments.
"
Class {
	#name : #RBSelectorEnvironment,
	#superclass : #RBBrowserEnvironmentWrapper,
	#instVars : [
		'classSelectors',
		'metaClassSelectors'
	],
	#category : #'Refactoring-Environment'
}

{ #category : #'instance creation' }
RBSelectorEnvironment class >> implementorsMatching: aString [
	^ self 
		implementorsMatching: aString 
		in: self default
]

{ #category : #'instance creation' }
RBSelectorEnvironment class >> implementorsMatching: aString in: anEnvironment [ 
	| classDict metaDict |
	classDict := IdentityDictionary new.
	metaDict := IdentityDictionary new.
	anEnvironment classesDo: [ :class | 
		| selectors |
		selectors := IdentitySet new.
		anEnvironment selectorsForClass: class
			do: [ :each | (aString match: each) ifTrue: [ selectors add: each ] ].
		selectors isEmpty ifFalse: [
			class isMeta 
				ifTrue: [ metaDict at: class soleInstance name put: selectors ]
				ifFalse: [ classDict at: class name put: selectors ] ] ].
	^ (self onEnvironment: anEnvironment)
		classSelectors: classDict metaClassSelectors: metaDict;
		label: 'Implementors of ' , aString;
		yourself
]

{ #category : #'instance creation' }
RBSelectorEnvironment class >> implementorsOf: aSelector [
	^ self
		implementorsOf: aSelector
		in: self default 
]

{ #category : #'instance creation' }
RBSelectorEnvironment class >> implementorsOf: aSelector in: anEnvironment [ 
	| classDict metaDict selectors |
	classDict := IdentityDictionary new.
	metaDict := IdentityDictionary new.
	selectors := IdentitySet with: aSelector.
	anEnvironment classesDo: [ :class | 
		((class includesLocalSelector: aSelector) and: [ anEnvironment includesSelector: aSelector in: class ]) 
			ifTrue: [
				class isMeta 
					ifTrue: [ metaDict at: class soleInstance name put: selectors copy ]
					ifFalse: [ classDict at: class name put: selectors copy ] ] ].
	^ (self onEnvironment: anEnvironment)
		classSelectors: classDict metaClassSelectors: metaDict;
		label: 'Implementors of ' , aSelector;
		yourself
]

{ #category : #'instance creation' }
RBSelectorEnvironment class >> matches: aString [
	^ self
		matches: aString
		in: self default
]

{ #category : #'instance creation' }
RBSelectorEnvironment class >> matches: aString in: anEnvironment [ 
	| newEnvironment |
	newEnvironment := (self onEnvironment: anEnvironment)
		label: 'Matching: ' , aString;
		searchStrings: (Array with: aString);
		yourself.
	anEnvironment classesAndSelectorsDo: [ :class :selector | 
		| method |
		method := class compiledMethodAt: selector.
		method allLiterals do: [ :literal | 
			literal isString ifTrue: [
				(aString match: literal)
					ifTrue: [ newEnvironment addClass: class selector: selector ] ] ] ].
	^ newEnvironment
]

{ #category : #'instance creation' }
RBSelectorEnvironment class >> onMethods: selectorCollection forClass: aClass [
	^ self
		onMethods: selectorCollection
		forClass: aClass
		in: self default 
]

{ #category : #'instance creation' }
RBSelectorEnvironment class >> onMethods: selectorCollection forClass: aClass in: anEnvironment [ 
	| environemnt |
	environemnt := self onEnvironment: anEnvironment.
	selectorCollection do: [ :each | environemnt addClass: aClass selector: each ].
	^ environemnt
]

{ #category : #'instance creation' }
RBSelectorEnvironment class >> referencesTo: aLiteral [
	^ self
		referencesTo: aLiteral
		in: self default 
]

{ #category : #'instance creation' }
RBSelectorEnvironment class >> referencesTo: aLiteral in: anEnvironment [ 
	| classDict literalPrintString |
	literalPrintString := aLiteral isVariableBinding
		ifTrue: [ aLiteral key asString ]
		ifFalse: [
			aLiteral isString
				ifTrue: [ aLiteral ]
				ifFalse: [ aLiteral printString ] ].
	classDict := IdentityDictionary new.
	anEnvironment classesDo: [ :class | 
		| selectors |
		selectors := (class thoroughWhichSelectorsReferTo: aLiteral)
			select: [ :selector | anEnvironment includesSelector: selector in: class ].
		selectors isEmpty 
			ifFalse: [ classDict at: class put: selectors asIdentitySet ] ].
	^ (self onEnvironment: anEnvironment)
		on: classDict;
		label: 'References to: ' , literalPrintString;
		searchStrings: (Array with: literalPrintString);
		yourself
]

{ #category : #adding }
RBSelectorEnvironment >> addClass: aClass [ 
	aClass isMeta 
		ifTrue: [ metaClassSelectors at: aClass soleInstance name put: aClass selectors asIdentitySet ]
		ifFalse: [ classSelectors at: aClass name put: aClass selectors asIdentitySet ]
]

{ #category : #adding }
RBSelectorEnvironment >> addClass: aClass selector: aSymbol [ 
	(aClass isMeta 
		ifTrue: [ metaClassSelectors at: aClass soleInstance name ifAbsentPut: [ IdentitySet new ] ]
		ifFalse: [ classSelectors at: aClass name ifAbsentPut: [ IdentitySet new ] ])
			add: aSymbol
]

{ #category : #adding }
RBSelectorEnvironment >> addMethod: aMethod [
	"this is a method to improve addition of methods, as class and selector data can be retrieved from a single compiled method"
	
	self addClass: aMethod methodClass selector: aMethod selector
]

{ #category : #accessing }
RBSelectorEnvironment >> asSelectorEnvironment [
	^ self
]

{ #category : #'accessing-classes' }
RBSelectorEnvironment >> classNames [
	^ IdentitySet new
		addAll: classSelectors keys;
		addAll: metaClassSelectors keys;
		yourself
]

{ #category : #'initialize-release' }
RBSelectorEnvironment >> classSelectors: classSelectorDictionary metaClassSelectors: metaClassSelectorDictionary [ 
	classSelectors := classSelectorDictionary.
	metaClassSelectors := metaClassSelectorDictionary
]

{ #category : #initialization }
RBSelectorEnvironment >> classes: classArray metaClasses: metaArray [ 
	"Used to recreate an environment from its storeString"

	classSelectors := IdentityDictionary new.
	metaClassSelectors := IdentityDictionary new.
	classArray 
		do: [ :each | classSelectors at: each first put: each last asIdentitySet ].
	metaArray 
		do: [ :each | metaClassSelectors at: each first put: each last asIdentitySet ]
]

{ #category : #'accessing-classes' }
RBSelectorEnvironment >> classesDo: aBlock [ 
	classSelectors keysDo: [ :each | 
		| class |
		class := self systemDictionary at: each ifAbsent: [ nil ].
		(class notNil and: [ environment includesClass: class ])
			ifTrue: [ aBlock value: class ] ].
	metaClassSelectors keysDo: [ :each | 
		| class |
		class := self systemDictionary at: each ifAbsent: [ nil ].
		(class notNil and: [ environment includesClass: class class ])
			ifTrue: [ aBlock value: class class ] ]
]

{ #category : #private }
RBSelectorEnvironment >> defaultLabel [
	^'some methods'
]

{ #category : #testing }
RBSelectorEnvironment >> includesCategory: aCategory [ 
	^(super includesCategory: aCategory) and: 
			[(self classNamesFor: aCategory) anySatisfy: 
					[:className | 
					(classSelectors includesKey: className) 
						or: [metaClassSelectors includesKey: className]]]
]

{ #category : #testing }
RBSelectorEnvironment >> includesClass: aClass [ 
	^(self privateSelectorsForClass: aClass) isNotEmpty 
		and: [super includesClass: aClass]
]

{ #category : #testing }
RBSelectorEnvironment >> includesProtocol: aProtocol in: aClass [ 
	^(super includesProtocol: aProtocol in: aClass)
		and: [(environment selectorsFor: aProtocol in: aClass)
				anySatisfy: [:aSelector | self privateIncludesSelector: aSelector inClass: aClass]]
]

{ #category : #testing }
RBSelectorEnvironment >> includesSelector: aSelector in: aClass [ 
	^(environment includesSelector: aSelector in: aClass)
		and: [self privateIncludesSelector: aSelector inClass: aClass]
]

{ #category : #initialization }
RBSelectorEnvironment >> initialize [
	super initialize.
	classSelectors := IdentityDictionary new.
	metaClassSelectors := IdentityDictionary new
]

{ #category : #testing }
RBSelectorEnvironment >> isEmpty [
	^classSelectors isEmpty and: [metaClassSelectors isEmpty]
]

{ #category : #testing }
RBSelectorEnvironment >> isSelectorEnvironment [
	^ true
]

{ #category : #initialization }
RBSelectorEnvironment >> on: aDictionary [ 
	aDictionary keysAndValuesDo: [ :class :selectors | 
		class isMeta 
			ifTrue: [ metaClassSelectors at: class soleInstance name put: selectors asIdentitySet ]
			ifFalse: [ classSelectors at: class name put: selectors asIdentitySet ] ]
]

{ #category : #accessing }
RBSelectorEnvironment >> packages [
	"Check that packages have really class and selector included."

	| pSet |
	pSet := Set new.
	self classes
		do: [ :each | 
			each packages
				do: [ :p | 
					self
						selectorsForClass: each
						do: [ :s | 
							(p includesSelector: s ofClass: each)
								ifTrue: [ pSet add: p ] ] ] ].
	^ pSet
]

{ #category : #copying }
RBSelectorEnvironment >> postCopy [
	| newDict |
	super postCopy.
	newDict := classSelectors copy.
	newDict keysAndValuesDo: [:key :value | newDict at: key put: value copy].
	classSelectors := newDict.
	newDict := metaClassSelectors copy.
	newDict keysAndValuesDo: [:key :value | newDict at: key put: value copy].
	metaClassSelectors := newDict
]

{ #category : #private }
RBSelectorEnvironment >> privateIncludesSelector: aSelector inClass: aClass [ 
	^(self privateSelectorsForClass: aClass) includes: aSelector
]

{ #category : #private }
RBSelectorEnvironment >> privateSelectorsForClass: aClass [ 
	^aClass isMeta 
		ifTrue: [metaClassSelectors at: aClass soleInstance name ifAbsent: [#()]]
		ifFalse: [classSelectors at: aClass name ifAbsent: [#()]]
]

{ #category : #removing }
RBSelectorEnvironment >> removeClass: aClass [ 
	aClass isMeta 
		ifTrue: [metaClassSelectors removeKey: aClass soleInstance name ifAbsent: []]
		ifFalse: [classSelectors removeKey: aClass name ifAbsent: []]
]

{ #category : #removing }
RBSelectorEnvironment >> removeClass: aClass selector: aSelector [ 
	(aClass isMeta 
		ifTrue: [metaClassSelectors at: aClass soleInstance name ifAbsent: [^self]]
		ifFalse: [classSelectors at: aClass name ifAbsent: [^self]]) 
			remove: aSelector
			ifAbsent: []
]

{ #category : #accessing }
RBSelectorEnvironment >> selectorsForClass: aClass do: aBlock [ 
	^(self privateSelectorsForClass: aClass) 
		do: [:each | (aClass includesSelector: each) ifTrue: [aBlock value: each]]
]

{ #category : #printing }
RBSelectorEnvironment >> storeOn: aStream [ 
	| classBlock |
	aStream
		nextPutAll: '((';
		nextPutAll: self class name;
		nextPutAll: ' onEnvironment: '.
	environment storeOn: aStream.
	aStream
		nextPut: $);
		nextPutAll: ' classes: #('.
	classBlock := 
			[:key :value | 
			aStream
				nextPutAll: '#(';
				nextPutAll: key;
				nextPutAll: ' #('.
			value do: 
					[:each | 
					aStream
						nextPutAll: each;
						nextPut: $ ].
			aStream
				nextPutAll: '))';
				cr].
	classSelectors keysAndValuesDo: classBlock.
	aStream nextPutAll: ') metaClasses: #('.
	metaClassSelectors keysAndValuesDo: classBlock.
	aStream nextPutAll: '))'
]
