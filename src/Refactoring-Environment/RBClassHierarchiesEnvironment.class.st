"
I am a RBBrowserEnvironment on a set of classes.
I contain all entities of this set.

Example:

```
RBClassHierarchiesEnvironment new forClasses: Number withAllSubclasses.
```
"
Class {
	#name : 'RBClassHierarchiesEnvironment',
	#superclass : 'RBBrowserEnvironmentWrapper',
	#instVars : [
		'classes',
		'metaClasses'
	],
	#category : 'Refactoring-Environment',
	#package : 'Refactoring-Environment'
}

{ #category : 'api' }
RBClassHierarchiesEnvironment class >> class: aClass [
	^ self classes: { aClass }
]

{ #category : 'api' }
RBClassHierarchiesEnvironment class >> classes: aCollection [
	^ self
		onEnvironment: self default
		classes: aCollection
]

{ #category : 'accessing - defaults' }
RBClassHierarchiesEnvironment class >> defaultName [

	^ 'Classes'
]

{ #category : 'api' }
RBClassHierarchiesEnvironment class >> onEnvironment: anEnvironment classes: aCollection [
	^(self onEnvironment: anEnvironment)
		classes: aCollection; yourself
]

{ #category : 'adding' }
RBClassHierarchiesEnvironment >> addClass: aClass [
	aClass isMeta
		ifTrue: [ metaClasses add: aClass soleInstance name ]
		ifFalse: [ classes add: aClass name ]
]

{ #category : 'adding' }
RBClassHierarchiesEnvironment >> addClassAndMetaClassFor: aClassOrMetaClass [
	self
		addClass: aClassOrMetaClass classSide;
		addClass: aClassOrMetaClass instanceSide
]

{ #category : 'accessing' }
RBClassHierarchiesEnvironment >> asSelectorEnvironment [

	^RBSelectorEnvironment new
		searchStrings:#();
		label:self label;
		onEnvironment: self environment;
		classSelectors: self classSelectorDictionary metaClassSelectors: self metaClassSelectorDictionary;
		yourself
]

{ #category : 'accessing' }
RBClassHierarchiesEnvironment >> basisObjects [
	^ classes
]

{ #category : 'accessing - classes' }
RBClassHierarchiesEnvironment >> classNames [
	^ IdentitySet new
		addAll: classes;
		addAll: metaClasses;
		yourself
]

{ #category : 'printing' }
RBClassHierarchiesEnvironment >> classSelectorDictionary [
	^ classes
		inject: (IdentityDictionary new: classes size)
		into: [ :answer :class |
			answer
				at: class put: (self systemDictionary at: class) selectors;
				yourself ]
]

{ #category : 'initialization' }
RBClassHierarchiesEnvironment >> classes: aCollection [
	aCollection do: [ :each | self addClass: each ]
]

{ #category : 'accessing - classes' }
RBClassHierarchiesEnvironment >> classesDo: aBlock [
	"calcular las classes incluidas en las jerarquias de cada class hierarchy"
	"se itera y se agrega a un set, por ende las repeticiones no importan"
	classes do: [ :each |
		| class |
		class := self systemDictionary at: each ifAbsent: [ nil ].
		(class isNotNil and: [ environment includesClass: class ])
			ifTrue: [ aBlock value: class ] ].
	metaClasses do: [ :each |
		| class |
		class := self systemDictionary at: each ifAbsent: [ nil ].
		(class isNotNil and: [ environment includesClass: class classSide ])
			ifTrue: [ aBlock value: class classSide ] ]
]

{ #category : 'private' }
RBClassHierarchiesEnvironment >> defaultLabel [
	| stream |
	stream := String new writeStream.
	classes do:
			[:each |
			stream nextPutAll: each;
				nextPut: $ ].
	^stream contents
]

{ #category : 'testing' }
RBClassHierarchiesEnvironment >> definesClass: aClass [

	^ self includesClass: aClass
]

{ #category : 'testing' }
RBClassHierarchiesEnvironment >> includesClass: aClass [
	^(aClass isMeta
		ifTrue: [metaClasses includes: aClass soleInstance name]
		ifFalse: [classes includes: aClass name])
			and: [super includesClass: aClass]
]

{ #category : 'initialization' }
RBClassHierarchiesEnvironment >> initialize [
	super initialize.
	classes := IdentitySet new.
	metaClasses := IdentitySet new
]

{ #category : 'testing' }
RBClassHierarchiesEnvironment >> isClassEnvironment [
	^true
]

{ #category : 'testing' }
RBClassHierarchiesEnvironment >> isEmpty [
	^classes isEmpty and: [metaClasses isEmpty]
]

{ #category : 'printing' }
RBClassHierarchiesEnvironment >> metaClassSelectorDictionary [
	^ metaClasses
		inject: (IdentityDictionary new: metaClasses size)
		into: [ :answer :class |
			answer
				at: class put: (self systemDictionary at: class) class selectors;
				yourself ]
]

{ #category : 'copying' }
RBClassHierarchiesEnvironment >> postCopy [
	super postCopy.
	classes := classes copy.
	metaClasses := metaClasses copy
]

{ #category : 'printing' }
RBClassHierarchiesEnvironment >> printBasisObject: aClass on: aStream [

	^ aStream nextPutAll: aClass
]

{ #category : 'printing' }
RBClassHierarchiesEnvironment >> printOn: aStream [

	aStream nextPutAll: 'Cls: '.
	classes do: [ :cls | aStream nextPutAll: cls, ', ']
]

{ #category : 'accessing' }
RBClassHierarchiesEnvironment >> problemCount [
	^self numberClasses
]

{ #category : 'removing' }
RBClassHierarchiesEnvironment >> removeClass: aClass [
	aClass isMeta
		ifTrue: [metaClasses remove: aClass soleInstance name ifAbsent: []]
		ifFalse: [classes remove: aClass name ifAbsent: []]
]

{ #category : 'accessing' }
RBClassHierarchiesEnvironment >> selectors [

	^ Array streamContents: [ :stream |
		self classesDo: [ :class |
			self selectorsForClass: class do: [ :selector | stream nextPut: selector ] ] ]
]

{ #category : 'printing' }
RBClassHierarchiesEnvironment >> storeOn: aStream [
	aStream nextPut: $(.
	super storeOn: aStream.
	aStream nextPutAll: ' classes: (('.
	classes asArray storeOn: aStream.
	aStream nextPutAll: ' inject: OrderedCollection new into: [:sum :each |
				| class |
				class := Smalltalk at: each ifAbsent: [nil].
				class notNil ifTrue: [sum add: class].
				sum]) , ('.
	metaClasses asArray storeOn: aStream.
	aStream nextPutAll: ' inject: OrderedCollection new into: [:sum :each |
				| class |
				class := Smalltalk at: each ifAbsent: [nil].
				class notNil ifTrue: [sum add: class class].
				sum])))'
]
