"
I am a namespace for refactoring operations.
I collect changes during refactoring and provide an environment for resolving named entities like
classes, method senders or implementors. 

The default environment for a namespace contains all classes in the system, but you can create 
an instance of me with a scoped environment to apply the refactoring operations only on entities in 
that environment.

Resolving named entities includes classes and methods changed or added during refactoring, before
actually applying this changes to the system. For this, I create model classes - RBClass and RBMetaclass - modeling real classes.

The changes that will be applied to the system are collected as refactoring changes (RBRefactoryChange).

Example: 
This creates a new namespace on the default environment (all system classes):

```
 model := RBNamespace onEnvironment: RBBrowserEnvironment new.
```

This creates a Refactoring operation for adding a class, with me as the namespace model:

```
    refactoring := RBAddClassRefactoring
        model: model
        addClass: #SomeClass
        superclass: #Object
        subclasses: {}
        category: #Category.
```

This will do a ""dry run"", only collect the changes of this refactoring:

```
    refactoring primitiveExecute.
```

Now can ask me for this list of changes:

```
   model changes.
```
this acutally gives a composite refactory change that holds a list of all single refacoring changes.

Note, some of my methods for querying method senders and implementors don't work well for Trait methods, because we don't have yet modeling classes for Traits and some refactoring operations don't support refactorings on trait methods.

"
Class {
	#name : 'RBNamespace',
	#superclass : 'RBEntity',
	#instVars : [
		'changes',
		'environment',
		'newClasses',
		'removedClasses',
		'changedClasses',
		'rootClasses',
		'implementorsCache',
		'sendersCache',
		'newPackages',
		'changedPackages',
		'removedPackages'
	],
	#category : 'Refactoring-Core-Model',
	#package : 'Refactoring-Core',
	#tag : 'Model'
}

{ #category : 'instance creation' }
RBNamespace class >> onEnvironment: aBrowserEnvironment [
	^self basicNew
		environment: aBrowserEnvironment;
		initialize;
		yourself
]

{ #category : 'private - changes' }
RBNamespace >> addChangeToClass: aRBClass [
	^ changedClasses
		at: aRBClass name
		put: (Array
				with: aRBClass instanceSide
				with: aRBClass classSide)
]

{ #category : 'private - changes' }
RBNamespace >> addClassVariable: aString to: aRBClass [
	^changes addClassVariable: aString to: aRBClass
]

{ #category : 'private - changes' }
RBNamespace >> addInstanceVariable: aString to: aRBClass [
	^changes addInstanceVariable: aString to: aRBClass
]

{ #category : 'private - changes' }
RBNamespace >> addPackageNamed: aSymbol [
	| change pkg |
	change := changes addPackageNamed: aSymbol.
	self flushCaches.
	pkg := self packageNamed: aSymbol.
	self unmarkPackageAsRemoved: aSymbol.
	newPackages at: aSymbol put: pkg.
	^change
]

{ #category : 'private - changes' }
RBNamespace >> addPool: aString to: aRBClass [
	^changes addPool: aString to: aRBClass
]

{ #category : 'adding' }
RBNamespace >> addProtocolNamed: aString in: aClass [

	^ changes addProtocolNamed: aString in: aClass
]

{ #category : 'accessing' }
RBNamespace >> allClassesDo: aBlock [

	| classes |
	classes := Set new: 4096.
	environment classesDo: [ :class | class isObsolete ifFalse: [self classFor: class]  ].
	classes addAll: newClasses values flattened.
	classes addAll: changedClasses value flattened.
	
	classes do: [ :rbClass |
			(self hasRemoved: (self classNameFor: rbClass)) ifFalse: [
			aBlock value: rbClass] ]
]

{ #category : 'accessing' }
RBNamespace >> allClassesInPackages: aColl do: aBlock [

	| seen evalBlock |
	seen := Set new.
	evalBlock := [ :class |
	             seen add: class first name.
	             aBlock
		             value: class first;
		             value: class last ].
	(newClasses select: [ :class |
		 | classPackage |
		 classPackage := class first packageName.
		 aColl anySatisfy: [ :packageName | packageName sameAs: classPackage ] ]) do: evalBlock.
	(changedClasses select: [ :class |
		 | classPackage |
		 classPackage := class first packageName.
		 aColl anySatisfy: [ :packageName | packageName sameAs: classPackage ] ]) do: evalBlock.
	self environment classesInPackages: aColl do: [ :each |
		each isObsolete ifFalse: [
			| class |
			class := each instanceSide.
			((seen includes: class name) or: [ self hasRemoved: (self classNameFor: class) ]) ifFalse: [
				(class := self classFor: each) ifNotNil: [
					seen add: class name.
					aBlock
						value: class;
						value: class classSide ] ] ] ]
]

{ #category : 'accessing' }
RBNamespace >> allImplementorsOf: aSelector [
	^ implementorsCache at: aSelector ifAbsentPut: [ self privateImplementorsOf: aSelector ]
]

{ #category : 'accessing' }
RBNamespace >> allImplementorsOf: aSelector do: aBlock [
	(self allImplementorsOf: aSelector) do: aBlock
]

{ #category : 'accessing' }
RBNamespace >> allImplementorsOf: aSelector inPackages: aColl [
	^ implementorsCache at: aSelector ifAbsentPut: [ self privateImplementorsOf: aSelector in: aColl ]
]

{ #category : 'accessing' }
RBNamespace >> allReferencesTo: aSymbol [
	^ sendersCache at: aSymbol ifAbsentPut: [ self privateReferencesTo: aSymbol ]
]

{ #category : 'accessing' }
RBNamespace >> allReferencesTo: aSymbol do: aBlock [
	(self allReferencesTo: aSymbol) do: aBlock
]

{ #category : 'accessing' }
RBNamespace >> allReferencesTo: aSymbol in: classes [
	| classNames |
	classNames := classes collect: [ :cls | cls name ].
	^ (self allReferencesTo: aSymbol) select: [ :ref | classNames includes: ref modelClass name ]
]

{ #category : 'accessing' }
RBNamespace >> allReferencesTo: aSymbol inPackages: aColl [
	^ sendersCache at: aSymbol ifAbsentPut: [ self privateReferencesTo: aSymbol inPackages: aColl ]
]

{ #category : 'accessing' }
RBNamespace >> allReferencesTo: aSymbol inPackages: aColl do: aBlock [
	(self allReferencesTo: aSymbol inPackages: aColl) do: aBlock
]

{ #category : 'accessing' }
RBNamespace >> allReferencesToClass: aRBClass do: aBlock [
	self allClassesDo:
			[ :each |
			(each whichSelectorsReferToClass: aRBClass)
				do: [:sel | aBlock value: (each methodFor: sel)] ]
]

{ #category : 'accessing' }
RBNamespace >> allReferencesToClass: aRBClass inPackages: aColl do: aBlock [
	self allClassesInPackages: aColl do:
			[:each |
			(each whichSelectorsReferToClass: aRBClass)
				do: [:sel | aBlock value: (each methodFor: sel)]]
]

{ #category : 'private - changes' }
RBNamespace >> changeClass: aRBClass [
	changedClasses
		at: aRBClass name
		put: (Array
				with: aRBClass instanceSide
				with: aRBClass classSide).
	self flushCaches
]

{ #category : 'accessing' }
RBNamespace >> changes [
	^changes
]

{ #category : 'accessing - classes' }
RBNamespace >> classFor: aBehavior [

	aBehavior ifNil: [ ^ nil ].
	^ aBehavior isMeta
		ifTrue: [ self metaclassNamed: aBehavior instanceSide name ]
		ifFalse: [ self classNamed: aBehavior instanceSide name ]
]

{ #category : 'private' }
RBNamespace >> classNameFor: aBehavior [
	^ aBehavior instanceSide name
]

{ #category : 'accessing - classes' }
RBNamespace >> classNamed: aSymbol [
	| class classes index symb |
	aSymbol ifNil: [ ^ nil ].
	symb := aSymbol asSymbol.
	(self hasRemoved: symb) ifTrue: [ ^ nil ].
	(newClasses includesKey: symb) ifTrue: [ ^ (newClasses at: symb) first ].
	(changedClasses includesKey: symb) ifTrue: [ ^ (changedClasses at: symb) first ].
	class := environment at: symb ifAbsent: [ nil ].
	(class isBehavior or: [ class isTrait ])
		ifTrue: [ classes := self createNewClassFor: class.
			^ class isMeta
				ifTrue: [ classes last ]
				ifFalse: [ classes first ] ].
	index := symb indexOfSubCollection: ' class' startingAt: 1 ifAbsent: [ ^ nil ].
	class := self classNamed: (symb copyFrom: 1 to: index - 1) asSymbol.
	^ class ifNotNil: [ class classSide ]
]

{ #category : 'converting' }
RBNamespace >> classObjectFor: anObject [

	(anObject isBehavior or: [anObject isTrait])
		ifTrue: [ ^ self classFor: anObject ].
	anObject isString
		ifTrue: [ ^ self classNamed: anObject ].
	^ anObject
]

{ #category : 'accessing - classes' }
RBNamespace >> classesReferencingClass: aRBClass [ 

	| methods |
	methods := Set new. 
	self allReferencesToClass: aRBClass do: [ :meth | methods add: meth methodClass ].
	^ methods
]

{ #category : 'changes' }
RBNamespace >> comment: aString in: aClass [
	^ changes comment: aString in: aClass
]

{ #category : 'private - changes' }
RBNamespace >> compile: aString in: aRBClass classified: aSymbol [
	| change |
	change := changes
				compile: aString
				in: aRBClass
				classified: aSymbol.
	self flushCaches.
	^change
]

{ #category : 'transforming' }
RBNamespace >> convertClasses: classSet select: aBlock using: searchReplacer [

	classSet do: [ :aClass |
		(aBlock value: aClass) do: [ :selector |
			aClass convertMethod: selector using: searchReplacer ]]
]

{ #category : 'accessing - classes' }
RBNamespace >> createNewClassFor: aBehavior [

	| nonMeta meta className rbType rbMetaType |
	className := aBehavior instanceSide name.
	aBehavior isTrait
		ifTrue: [
			rbType := self rbTraitClass.
			rbMetaType := self rbTraitedMetaclassClass ]
		ifFalse: [
			rbType := self rbClassClass.
			rbMetaType := self rbMetaclassClass ].
	nonMeta := rbType existingNamed: className model: self.
	meta := rbMetaType existingNamed: className model: self.
	nonMeta packageName: aBehavior package name.
	aBehavior packageTag isRoot ifFalse: [ nonMeta tagName: aBehavior packageTag name ].
	^ changedClasses at: className put: (Array with: nonMeta with: meta)
]

{ #category : 'accessing - classes' }
RBNamespace >> createNewPackageFor: aPackage [
	| package |
	package := self rbPackageClass existingNamed: aPackage model: self.
	^changedPackages at: aPackage put: (Array with: package)
]

{ #category : 'changes' }
RBNamespace >> defineClass: aBlock [

	| change newClass newClassName |
	change := changes defineClass: aBlock.
	newClassName := change changeClassName.
	newClass := self classNamed: newClassName.
	newClass ifNil: [
		| newMetaclass |
		self unmarkAsRemoved: newClassName.
		newClass := self rbClassClass named: newClassName.
		newMetaclass := self rbMetaclassClass named: newClassName.
		newClass model: self.
		newMetaclass model: self.
		newClasses at: newClassName put: (Array with: newClass with: newMetaclass) ].
	newClass superclass: (self classNamed: change superclassName).
	newClass superclass
		ifNil: [
			self rootClasses add: newClass.
			newClass classSide superclass: (self classFor: Object class superclass) ]
		ifNotNil: [ newClass classSide superclass: newClass superclass classSide ].
	newClass instanceVariableNames: (change instanceVariableNames ifNil: [ '' ]).
	newClass classVariableNames: (change classVariableNames ifNil: [ '' ]).
	newClass poolDictionaryNames: (change sharedPoolNames ifNil: [ '' ]).
	change comment ifNotNil: [ :comment | newClass comment: comment ].
	newClass packageName: change package.
	newClass tagName: change tag.
	^ change
]

{ #category : 'accessing' }
RBNamespace >> description [
	^ self changes name
]

{ #category : 'accessing' }
RBNamespace >> description: aString [
	self changes name: aString
]

{ #category : 'accessing' }
RBNamespace >> environment [
	^ environment ifNil: [ environment := RBBrowserEnvironment new]
]

{ #category : 'accessing' }
RBNamespace >> environment: aBrowserEnvironment [
	environment := aBrowserEnvironment
]

{ #category : 'private - changes' }
RBNamespace >> flushCaches [
	implementorsCache := IdentityDictionary new.
	sendersCache := IdentityDictionary new
]

{ #category : 'private' }
RBNamespace >> hasCreatedClassFor: aBehavior [
	| className |
	className := self classNameFor: aBehavior.
	^(newClasses includesKey: className)
		or: [changedClasses includesKey: className]
]

{ #category : 'testing' }
RBNamespace >> hasPackageRemoved: aSymbol [
	^removedPackages includes: aSymbol
]

{ #category : 'testing' }
RBNamespace >> hasRemoved: aSymbol [
	^removedClasses includes: aSymbol
]

{ #category : 'testing' }
RBNamespace >> includesClassNamed: aSymbol [
	^(self classNamed: aSymbol) isNotNil
]

{ #category : 'testing' }
RBNamespace >> includesGlobal: aSymbol [
	(self hasRemoved: aSymbol) ifTrue: [^false].
	(self includesClassNamed: aSymbol) ifTrue: [^true].
	environment at: aSymbol ifAbsent: [^false].
	^true
]

{ #category : 'testing' }
RBNamespace >> includesPackageNamed: aSymbol [

	^ (self packageNamed: aSymbol) isNotNil
]

{ #category : 'initialization' }
RBNamespace >> initialize [
	super initialize.
	changes := changeFactory compositeRefactoryChange onSystemEnvironment: self environment.
	newClasses := IdentityDictionary new.
	newPackages := IdentityDictionary new.
	changedClasses := IdentityDictionary new.
	changedPackages := IdentityDictionary new.
	removedClasses := Set new.
	removedPackages := Set new.
	implementorsCache := IdentityDictionary new.
	sendersCache := IdentityDictionary new
]

{ #category : 'private' }
RBNamespace >> markAsRemoved: aClassName [

	removedClasses
		add: aClassName;
		add: aClassName, ' class'
]

{ #category : 'private' }
RBNamespace >> markPackageAsRemoved: aPackageName [

	removedPackages
		add: aPackageName
]

{ #category : 'accessing - classes' }
RBNamespace >> metaclassNamed: aSymbol [

	| class |

	aSymbol ifNil: [ ^ nil ].
	( self hasRemoved: aSymbol )
		ifTrue: [ ^ nil ].
	( newClasses includesKey: aSymbol )
		ifTrue: [ ^ ( newClasses at: aSymbol ) last ].
	( changedClasses includesKey: aSymbol )
		ifTrue: [ ^ ( changedClasses at: aSymbol ) last ].

	class := environment at: aSymbol ifAbsent: [ nil ].
	( class isBehavior or: [ class isTrait ] )
		ifTrue: [ ^ ( self createNewClassFor: class ) last ].
	^ nil
]

{ #category : 'accessing - classes' }
RBNamespace >> methodsReferencingClass: aRBClass [ 

	| methods |
	methods := Set new. 
	self allReferencesToClass: aRBClass do: [ :meth | methods add: meth ].
	^ methods
]

{ #category : 'accessing' }
RBNamespace >> name [
	^changes name
]

{ #category : 'accessing' }
RBNamespace >> name: aString [
	^changes name: aString
]

{ #category : 'accessing - classes' }
RBNamespace >> packageNamed: aSymbol [
	aSymbol ifNil: [ ^ nil ].
	(self hasPackageRemoved: aSymbol) ifTrue: [ ^ nil ].
	(newPackages includesKey: aSymbol) ifTrue: [ ^ newPackages at: aSymbol ].
	(changedPackages includesKey: aSymbol) ifTrue: [ ^ (changedPackages at: aSymbol) first ].
	^ (self createNewPackageFor: aSymbol) first
]

{ #category : 'private - changes' }
RBNamespace >> performChange: aCompositeRefactoryChange around: aBlock [
	| oldChanges |
	changes addChange: aCompositeRefactoryChange.
	oldChanges := changes.
	changes := aCompositeRefactoryChange.
	aBlock ensure: [changes := oldChanges].
	^aCompositeRefactoryChange
]

{ #category : 'private' }
RBNamespace >> privateImplementorsOf: aSelector [
	| classes |
	classes := Set new.
	self allClassesDo: [ :class |
		(class directlyDefinesLocalMethod: aSelector)
			ifTrue: [ classes add: class ] ].
	^ classes
]

{ #category : 'private' }
RBNamespace >> privateImplementorsOf: aSelector in: packages [
	| classes |
	classes := Set new.
	self allClassesInPackages: packages do: [ :class |
		(class directlyDefinesLocalMethod: aSelector)
			ifTrue: [ classes add: class ] ].
	^ classes
]

{ #category : 'private' }
RBNamespace >> privateReferencesTo: aSelector [
	| methods |
	methods := OrderedCollection new.
	self allClassesDo: [ :class |
		(class whichSelectorsReferToSymbol: aSelector)
			do: [ :selector |
				methods add: (class methodFor: selector) ] ].
	^ methods
]

{ #category : 'private' }
RBNamespace >> privateReferencesTo: aSelector in: classes [
	| methods |
	methods := OrderedCollection new.
	classes do: [ :class |
		(class whichSelectorsReferToSymbol: aSelector)
			do: [ :selector |
				methods add: (class methodFor: selector) ] ].
	^ methods
]

{ #category : 'private' }
RBNamespace >> privateReferencesTo: aSelector inPackages: aColl [
	| methods |
	methods := OrderedCollection new.
	self allClassesInPackages: aColl do: [ :class |
		(class whichSelectorsReferToSymbol: aSelector)
			do: [ :selector |
				methods add: (class methodFor: selector) ] ].
	^ methods
]

{ #category : 'private' }
RBNamespace >> privateRootClasses [

	| classes |
	classes := OrderedCollection new.
	{ ProtoObject } do: [ :each |
		| class |
		class := self classFor: each.
		(class isNotNil and: [ class superclass isNil ]) ifTrue: [
			classes add: class ] ].
	^ classes
]

{ #category : 'changes' }
RBNamespace >> removeClass: aRBClass [
	self removeClassNamed: aRBClass name
]

{ #category : 'private - changes' }
RBNamespace >> removeClassKeepingSubclassesNamed: aSymbol [
	removedClasses
		add: aSymbol;
		add: aSymbol , ' class'.
	newClasses removeKey: aSymbol ifAbsent: [].
	changedClasses removeKey: aSymbol ifAbsent: [].
	self flushCaches.
	^changes removeClassNamed: aSymbol
]

{ #category : 'changes' }
RBNamespace >> removeClassNamed: aSymbol [
	(self classNamed: aSymbol) subclasses
		do: [:each | self removeClassNamed: each name].
	removedClasses
		add: aSymbol;
		add: aSymbol , ' class'.
	newClasses removeKey: aSymbol ifAbsent: [].
	changedClasses removeKey: aSymbol ifAbsent: [].
	self flushCaches.
	^changes removeClassNamed: aSymbol
]

{ #category : 'private - changes' }
RBNamespace >> removeClassVariable: aString from: aRBClass [
	^changes removeClassVariable: aString from: aRBClass
]

{ #category : 'private - changes' }
RBNamespace >> removeInstanceVariable: aString from: aRBClass [
	^changes removeInstanceVariable: aString from: aRBClass
]

{ #category : 'private - changes' }
RBNamespace >> removeMethod: aSelector from: aRBClass [
	self flushCaches.
	^changes removeMethod: aSelector from: aRBClass
]

{ #category : 'private - changes' }
RBNamespace >> removePackageNamed: aString [

	self flushCaches.
	^ changes removePackageNamed: aString
]

{ #category : 'private - changes' }
RBNamespace >> removeProtocolNamed: aString in: aClass [

	^ changes removeProtocolNamed: aString in: aClass
]

{ #category : 'changes' }
RBNamespace >> renameClass: aRBClass to: aSymbol around: aBlock [
	| change value dict |
	change := changeFactory renameClass: aRBClass to: aSymbol.
	self performChange: change around: aBlock.
	self flushCaches.
	dict := (newClasses includesKey: aRBClass name)
		ifTrue: [ newClasses ]
		ifFalse: [ changedClasses ].
	self markAsRemoved: aRBClass name.
	self unmarkAsRemoved: aSymbol.
	value := dict at: aRBClass name.
	dict removeKey: aRBClass name.
	dict at: aSymbol put: value.
	value first name: aSymbol.
	value last name: aSymbol.
	value first subclasses do: [ :each | each superclass: value first ].
	value last subclasses do: [ :each | each superclass: value last ].
	^ change
]

{ #category : 'private - changes' }
RBNamespace >> renameInstanceVariable: oldName to: newName in: aRBClass around: aBlock [
	^self performChange: (changeFactory
				renameInstanceVariable: oldName
				to: newName
				in: aRBClass)
		around: aBlock
]

{ #category : 'changes' }
RBNamespace >> renamePackage: aRBPackage to: aSymbol [
	| change value dict |
	change := changeFactory renamePackage: aRBPackage to: aSymbol.
	self performChange: change around: [].
	self flushCaches.
	dict := changedPackages.
	self markPackageAsRemoved: aRBPackage name.
	self unmarkPackageAsRemoved: aSymbol.
	value := dict at: aRBPackage name.
	dict removeKey: aRBPackage name.
	dict at: aSymbol put: value.
	value first name: aSymbol.
	^ change
]

{ #category : 'accessing' }
RBNamespace >> repackage: aClass in: aPackage tag: aTag [

	^ changes addChange: (RBClassRepackagingChange repackage: aClass in: aPackage tag: aTag)
]

{ #category : 'changes' }
RBNamespace >> reparentClasses: aRBClassCollection to: newClass [

	aRBClassCollection do: [ :aClass |
		self defineClass: [ :aBuilder |
			aBuilder
				superclassName: newClass name;
				name: aClass name;
				slots: aClass instanceVariableNames;
				sharedVariables: aClass classVariableNames;
				sharedPools: aClass sharedPoolNames;
				package: aClass packageName;
				tag: aClass tagName ] ]
]

{ #category : 'accessing' }
RBNamespace >> rootClasses [
	^ rootClasses ifNil: [ rootClasses := self privateRootClasses]
]

{ #category : 'transforming' }
RBNamespace >> selector: aSelector in: aClass classified: aProtocol [

	^ changes
		selector: aSelector
		in: aClass
		classified: aProtocol
]

{ #category : 'private' }
RBNamespace >> unmarkAsRemoved: newClassName [
	removedClasses
		remove: newClassName ifAbsent: [  ];
		remove: newClassName , ' class' ifAbsent: [  ]
]

{ #category : 'private' }
RBNamespace >> unmarkPackageAsRemoved: newPackageName [
	removedPackages
		remove: newPackageName ifAbsent: [  ]
]
