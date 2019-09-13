"
I am a RBBrowserEnvironment for items referring class or instvars.
Constructed by quering extisting environments with 
refering, reading or writing to the variables of a class.

Example:
(RBBrowserEnvironment new) instVarWritersTo:#color in: Morph.
-> a RBVariableEnvironment
"
Class {
	#name : #RBVariableEnvironment,
	#superclass : #RBBrowserEnvironmentWrapper,
	#instVars : [
		'instanceVariables',
		'instanceVariableReaders',
		'instanceVariableWriters',
		'classVariables',
		'selectorCache'
	],
	#category : #'Refactoring-Environment'
}

{ #category : #'instance creation' }
RBVariableEnvironment class >> on: anEnvironment readersOfInstanceVariable: aString in: aClass [ 
	| newEnv |
	newEnv := (self onEnvironment: anEnvironment)
				label: 'Readers of ''' , aString , ''' in ' , aClass name;
				yourself.
	(aClass whichClassDefinesInstVar: aString) withAllSubclassesDo: 
			[:cls | 
			(cls whichSelectorsRead: aString) isEmpty
				ifFalse: [newEnv addClass: cls instanceVariableReader: aString]].
	^newEnv
]

{ #category : #'instance creation' }
RBVariableEnvironment class >> on: anEnvironment referencesToClassVariable: aSymbol in: aClass [ 
	| newEnv definingClass assoc |
	newEnv := (self onEnvironment: anEnvironment)
				label: 'References to ''' , aSymbol , ''' in ' , aClass name;
				yourself.
	definingClass := aClass whichClassDefinesClassVar: aSymbol.
	assoc := definingClass bindingOf: aSymbol.
	definingClass withAllSubclassesDo: 
			[:cls | 
			((cls whichSelectorsReferTo: assoc) notEmpty or:[ (cls classSide whichSelectorsReferTo: assoc) notEmpty]) 
				ifTrue: [newEnv addClass: cls classVariable: aSymbol]].
	^newEnv
]

{ #category : #'instance creation' }
RBVariableEnvironment class >> on: anEnvironment referencesToInstanceVariable: aString in: aClass [ 
	| newEnv |
	newEnv := (self onEnvironment: anEnvironment)
				label: 'References to ''' , aString , ''' in ' , aClass name;
				yourself.
	(aClass whichClassDefinesInstVar: aString) withAllSubclassesDo: 
			[:cls | 
			((cls whichSelectorsRead: aString) isEmpty not 
				or: [(cls whichSelectorsStoreInto: aString) isEmpty not]) 
					ifTrue: [newEnv addClass: cls instanceVariable: aString]].
	^newEnv
]

{ #category : #'instance creation' }
RBVariableEnvironment class >> on: anEnvironment writersOfInstanceVariable: aString in: aClass [ 
	| newEnv |
	newEnv := (self onEnvironment: anEnvironment)
				label: 'Writers of ''' , aString , ''' in ' , aClass name;
				yourself.
	(aClass whichClassDefinesInstVar: aString) withAllSubclassesDo: 
			[:cls | 
			(cls whichSelectorsStoreInto: aString) isEmpty
				ifFalse: [newEnv addClass: cls instanceVariableWriter: aString]].
	^newEnv
]

{ #category : #'instance creation' }
RBVariableEnvironment class >> readersOfInstanceVariable: aString in: aClass [ 
	^ self
		on: self default
		readersOfInstanceVariable: aString
		in: aClass
]

{ #category : #'instance creation' }
RBVariableEnvironment class >> referencesToClassVariable: aSymbol in: aClass [ 
	^ self 
		on: self default
		referencesToClassVariable: aSymbol 
		in: aClass 
]

{ #category : #'instance creation' }
RBVariableEnvironment class >> referencesToInstanceVariable: aString in: aClass [ 
	^ self
		on: self default 
		referencesToInstanceVariable: aString 
		in: aClass 
]

{ #category : #'instance creation' }
RBVariableEnvironment class >> writersOfInstanceVariable: aString in: aClass [ 
	^ self
		on: self default
		writersOfInstanceVariable: aString 
		in: aClass 
]

{ #category : #private }
RBVariableEnvironment >> accessorMethods [
	^#(#instanceVariables #instanceVariableReaders #instanceVariableWriters #classVariables)
]

{ #category : #accessing }
RBVariableEnvironment >> addClass: aClass classVariable: aSymbol [ 
	(classVariables at: aClass name ifAbsentPut: [Set new]) add: aSymbol.
	self flushCachesFor: aClass.
	self addSearchString: aSymbol
]

{ #category : #accessing }
RBVariableEnvironment >> addClass: aClass instanceVariable: aString [ 
	(instanceVariables at: aClass name ifAbsentPut: [Set new]) add: aString.
	self flushCachesFor: aClass.
	self addSearchString: aString
]

{ #category : #accessing }
RBVariableEnvironment >> addClass: aClass instanceVariableReader: aString [ 
	(instanceVariableReaders at: aClass name ifAbsentPut: [Set new]) 
		add: aString.
	self flushCachesFor: aClass.
	self addSearchString: aString
]

{ #category : #accessing }
RBVariableEnvironment >> addClass: aClass instanceVariableWriter: aString [ 
	(instanceVariableWriters at: aClass name ifAbsentPut: [Set new]) 
		add: aString.
	self flushCachesFor: aClass.
	self addSearchString: aString
]

{ #category : #private }
RBVariableEnvironment >> allClassesDo: aBlock [ 
	| classes instVarBlock |
	classes := Set new.
	instVarBlock := 
			[:each | 
			| class |
			class := self classForName: each.
			classes addAll: class withAllSubclasses].
	instanceVariables keysDo: instVarBlock.
	instanceVariableReaders keysDo: instVarBlock.
	instanceVariableWriters keysDo: instVarBlock.
	classVariables keysDo: 
			[:each | 
			| class |
			class := self classForName: each.
			class notNil 
				ifTrue: 
					[classes
						addAll: class withAllSubclasses;
						addAll: class class withAllSubclasses]].
	classes do: aBlock
]

{ #category : #private }
RBVariableEnvironment >> classForName: aString [ 
	| name isMeta class |
	isMeta := aString includes: $ .
	name := (isMeta 
		ifTrue: [ aString copyFrom: 1 to: (aString size - 6 max: 1) ]
		ifFalse: [ aString ]) asSymbol.
	class := self systemDictionary at: name ifAbsent: [ nil ].
	^ (class notNil and: [ isMeta ])
		ifTrue: [ class class ]
		ifFalse: [ class ]
]

{ #category : #accessing }
RBVariableEnvironment >> classNamesWithVariables [
	| classNames |
	classNames := Set new.
	classNames
		addAll: instanceVariables keys;
		addAll: instanceVariableReaders keys;
		addAll: instanceVariableWriters keys;
		addAll: classVariables keys.
	^classNames
]

{ #category : #private }
RBVariableEnvironment >> classVariableSelectorsFor: aClass [ 
	| selectors classVars |
	selectors := Set new.
	classVars := Set new.
	classVariables keysDo: 
			[:each | 
			| cls |
			cls := self classForName: each.
			(cls notNil and: [aClass instanceSide includesBehavior: cls]) 
				ifTrue: [classVars addAll: (classVariables at: each)]].
	classVars do: 
			[:each | 
			| binding |
			binding := aClass bindingOf: each.
			binding ifNotNil: [selectors addAll: (aClass whichSelectorsReferTo: binding)]].
	^selectors
]

{ #category : #private }
RBVariableEnvironment >> classVariables [
	^classVariables
]

{ #category : #private }
RBVariableEnvironment >> classVariables: anObject [
	classVariables := anObject
]

{ #category : #accessing }
RBVariableEnvironment >> classVariablesFor: aClass [ 
	^classVariables at: aClass name ifAbsent: [#()]
]

{ #category : #private }
RBVariableEnvironment >> computeSelectorCacheFor: aClass [ 
	^(self instanceVariableSelectorsFor: aClass)
		addAll: (self classVariableSelectorsFor: aClass);
		yourself
]

{ #category : #copying }
RBVariableEnvironment >> copyDictionary: aDictionary [ 
	| copy |
	copy := Dictionary new: aDictionary size.
	aDictionary keysAndValuesDo: [:key :value | copy at: key put: value].
	^copy
]

{ #category : #accessing }
RBVariableEnvironment >> environmentForClassVariable: aSymbol in: aClass [ 
	| selectorEnvironment assoc block |
	selectorEnvironment := RBSelectorEnvironment onEnvironment: self.
	selectorEnvironment addSearchString: aSymbol.
	((classVariables at: aClass name ifAbsent: [#()]) includes: aSymbol) 
		ifFalse: [^selectorEnvironment].
	assoc := aClass bindingOf: aSymbol.
	block := 
			[:each | 
			(each whichSelectorsReferTo: assoc) 
				do: [:sel | selectorEnvironment addClass: each selector: sel]].
	aClass withAllSubAndSuperclassesDo: 
			[:each | 
			block
				value: each;
				value: each class].
	^selectorEnvironment
]

{ #category : #accessing }
RBVariableEnvironment >> environmentForInstanceVariable: aString in: aClass [ 
	| selectorEnvironment isReader isWriter |
	selectorEnvironment := RBSelectorEnvironment onEnvironment: self.
	selectorEnvironment addSearchString: aString.
	isReader := isWriter := false.
	((instanceVariables at: aClass name ifAbsent: [#()]) includes: aString) 
		ifTrue: 
			[isReader := true.
			isWriter := true].
	((instanceVariableWriters at: aClass name ifAbsent: [#()]) 
		includes: aString) ifTrue: [isWriter := true].
	((instanceVariableReaders at: aClass name ifAbsent: [#()]) 
		includes: aString) ifTrue: [isReader := true].
	aClass withAllSubAndSuperclassesDo:
			[:each | 
			isWriter 
				ifTrue: 
					[(each whichSelectorsStoreInto: aString) 
						do: [:sel | selectorEnvironment addClass: each selector: sel]].
			isReader 
				ifTrue: 
					[(each whichSelectorsRead: aString) 
						do: [:sel | selectorEnvironment addClass: each selector: sel]]].
	^selectorEnvironment
]

{ #category : #private }
RBVariableEnvironment >> flushCachesFor: aClass [

	selectorCache ifNil: [ ^ self ].
	aClass instanceSide withAllSubclasses
		do: [ :each | 
			selectorCache
				removeKey: each name ifAbsent: [  ];
				removeKey: each classSide name ifAbsent: [  ]
			]
]

{ #category : #testing }
RBVariableEnvironment >> includesCategory: aCategory [ 
	^(self classNamesFor: aCategory) isNotEmpty
]

{ #category : #testing }
RBVariableEnvironment >> includesClass: aClass [ 
	(super includesClass: aClass) ifFalse: [^false].
	(instanceVariables includesKey: aClass name) ifTrue: [^true].
	(classVariables includesKey: aClass name) ifTrue: [^true].
	^(self selectorCacheFor: aClass) 
		anySatisfy: [:each | self includesSelector: each in: aClass]
]

{ #category : #testing }
RBVariableEnvironment >> includesProtocol: aProtocol in: aClass [ 
	^(self selectorsFor: aProtocol in: aClass) isNotEmpty
]

{ #category : #testing }
RBVariableEnvironment >> includesSelector: aSymbol in: aClass [ 
	^(environment includesSelector: aSymbol in: aClass) 
		and: [(self selectorCacheFor: aClass) includes: aSymbol]
]

{ #category : #initialization }
RBVariableEnvironment >> initialize [
	super initialize.
	instanceVariables := Dictionary new.
	classVariables := Dictionary new.
	instanceVariableReaders := Dictionary new.
	instanceVariableWriters := Dictionary new
]

{ #category : #private }
RBVariableEnvironment >> instanceVariableReaders [
	^instanceVariableReaders
]

{ #category : #private }
RBVariableEnvironment >> instanceVariableReaders: anObject [
	instanceVariableReaders := anObject
]

{ #category : #private }
RBVariableEnvironment >> instanceVariableSelectorsFor: aClass [ 
	| selectors |
	selectors := Set new.
	#(#instanceVariables #instanceVariableReaders #instanceVariableWriters) 
		with: #(#whichSelectorsAccess: #whichSelectorsRead: #whichSelectorsStoreInto:)
		do: [:var :sel | 
			| instVars |
			instVars := Set new.
			(self perform: var) keysDo: 
					[:each | 
					| cls |
					cls := self classForName: each.
					(cls notNil and: [aClass includesBehavior: cls]) 
						ifTrue: [instVars addAll: ((self perform: var) at: each)]].
			instVars do: [:each | selectors addAll: (aClass perform: sel with: each)]].
	^selectors
]

{ #category : #private }
RBVariableEnvironment >> instanceVariableWriters [
	^instanceVariableWriters
]

{ #category : #private }
RBVariableEnvironment >> instanceVariableWriters: anObject [
	instanceVariableWriters := anObject
]

{ #category : #private }
RBVariableEnvironment >> instanceVariables [
	^instanceVariables
]

{ #category : #private }
RBVariableEnvironment >> instanceVariables: anObject [
	instanceVariables := anObject
]

{ #category : #accessing }
RBVariableEnvironment >> instanceVariablesFor: aClass [ 
	| vars name |
	vars := Set new.
	name := aClass name.
	vars
		addAll: (instanceVariables at: name ifAbsent: [#()]);
		addAll: (instanceVariableReaders at: name ifAbsent: [#()]);
		addAll: (instanceVariableWriters at: name ifAbsent: [#()]).
	^vars
]

{ #category : #testing }
RBVariableEnvironment >> isEmpty [
	^ self accessorMethods allSatisfy: [ :each | (self perform: each) isEmpty ]
]

{ #category : #testing }
RBVariableEnvironment >> isVariableEnvironment [
	^ true
]

{ #category : #accessing }
RBVariableEnvironment >> numberVariables [
	^self accessorMethods inject: 0
		into: [:sum :each | sum + ((self perform: each) inject: 0 into: [:s :e | s + e size])]
]

{ #category : #accessing }
RBVariableEnvironment >> packages [
	^ self classes collect: #package
]

{ #category : #copying }
RBVariableEnvironment >> postCopy [
	super postCopy.
	instanceVariables := self copyDictionary: instanceVariables.
	instanceVariableReaders := self copyDictionary: instanceVariableReaders.
	instanceVariableWriters := self copyDictionary: instanceVariableWriters.
	classVariables := self copyDictionary: classVariables.
	selectorCache := nil
]

{ #category : #accessing }
RBVariableEnvironment >> problemCount [
	^self numberVariables
]

{ #category : #accessing }
RBVariableEnvironment >> removeClass: aClass classVariable: aSymbol [ 
	| vars |
	vars := classVariables at: aClass name ifAbsent: [Set new].
	vars remove: aSymbol ifAbsent: [].
	vars ifEmpty: [classVariables removeKey: aClass name ifAbsent: []].
	self flushCachesFor: aClass
]

{ #category : #accessing }
RBVariableEnvironment >> removeClass: aClass instanceVariable: aString [ 
	| vars |
	vars := instanceVariables at: aClass name ifAbsent: [Set new].
	vars remove: aString ifAbsent: [].
	vars ifEmpty: [instanceVariables removeKey: aClass name ifAbsent: []].
	self flushCachesFor: aClass
]

{ #category : #accessing }
RBVariableEnvironment >> removeClass: aClass instanceVariableReader: aString [ 
	| vars |
	vars := instanceVariableReaders at: aClass name ifAbsent: [Set new].
	vars remove: aString ifAbsent: [].
	vars ifEmpty: [instanceVariableReaders removeKey: aClass name ifAbsent: []].
	self flushCachesFor: aClass
]

{ #category : #accessing }
RBVariableEnvironment >> removeClass: aClass instanceVariableWriter: aString [ 
	| vars |
	vars := instanceVariableWriters at: aClass name ifAbsent: [Set new].
	vars remove: aString ifAbsent: [].
	vars ifEmpty: [instanceVariableWriters removeKey: aClass name ifAbsent: []].
	self flushCachesFor: aClass
]

{ #category : #private }
RBVariableEnvironment >> selectorCache [

	^ selectorCache ifNil: [ selectorCache := Dictionary new ] ifNotNil: [ selectorCache ]
]

{ #category : #private }
RBVariableEnvironment >> selectorCacheFor: aClass [ 
	^self selectorCache at: aClass name ifAbsentPut: [ self computeSelectorCacheFor: aClass ]
]

{ #category : #printing }
RBVariableEnvironment >> storeOn: aStream [ 
	aStream
		nextPut: $(;
		nextPutAll: self class name;
		nextPutAll: ' new '.
	self accessorMethods do: [ :each | 
		aStream
			nextPutAll: each;
			nextPutAll: ': '.
		(self perform: each) storeOn: aStream.
		aStream nextPutAll: '; '].
	aStream nextPutAll: 'yourself)'
]
