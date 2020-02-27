Class {
	#name : #RGClassDescripitonStrategyTest,
	#superclass : #RGTest,
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
RGClassDescripitonStrategyTest >> checkBasicPropertiesOf: anRGBehavior [

	self assert: anRGBehavior isBehavior.	
	self assert: anRGBehavior behaviorStrategy isClassStrategy.
	self assert: anRGBehavior behaviorStrategy owner equals: anRGBehavior.
	self assert: anRGBehavior layout isRingResolved not.
	self assert: anRGBehavior layout isFixedLayout.

	
]

{ #category : #tests }
RGClassDescripitonStrategyTest >> testAllSlots [

	| env aSuperclass aSubclass |
	
	env := RGEnvironment new.
	aSuperclass := env ensureClassNamed: 'Superclass'.
	aSubclass := env ensureClassNamed: 'Subclass'.
	aSubclass superclass: aSuperclass.

	aSuperclass instanceVariables: #(a b c).
	aSubclass instanceVariables: #(d e f).
	
	self assert: (aSubclass allSlots collect: #name) equals: #(a b c d e f).
]

{ #category : #tests }
RGClassDescripitonStrategyTest >> testClassDescriptionMakeResolved [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newClass.
	
	self deny: (anRGBehavior hasResolved: #layout).
	anRGBehavior makeResolved.
	self assert: (anRGBehavior hasResolved: #layout).
	
	
]

{ #category : #tests }
RGClassDescripitonStrategyTest >> testClassDescriptionResolvableProperties [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newClass.
	
	self assert: (anRGBehavior pvtResolvableProperties asDictionary includesKey: #layout)
]

{ #category : #tests }
RGClassDescripitonStrategyTest >> testCopyForBehaviorDefinitionPostCopy [
	| anRGBehavior copy |
	anRGBehavior := RGBehavior newClass.
	copy := anRGBehavior copyForBehaviorDefinition.
	self deny: anRGBehavior behaviorStrategy identicalTo: copy behaviorStrategy.
	self deny: (anRGBehavior behaviorStrategy instVarNamed: #layout) identicalTo: (copy behaviorStrategy instVarNamed: #layout)
	"TODO: check this failing case:"
	"	self deny: anRGBehavior behaviorStrategy layout == copy behaviorStrategy layout."
]

{ #category : #tests }
RGClassDescripitonStrategyTest >> testCreationByMethod [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newClass.
	self checkBasicPropertiesOf: anRGBehavior
]

{ #category : #tests }
RGClassDescripitonStrategyTest >> testCreationByNamed [

	| anRGBehavior |
	
	anRGBehavior := RGClass named: #SomeClass.
	self checkBasicPropertiesOf: anRGBehavior.
	self assert: anRGBehavior name equals: #SomeClass.

]

{ #category : #tests }
RGClassDescripitonStrategyTest >> testCreationByNamedParent [

	| anRGBehavior parent |
	
	parent := RGEnvironment	new.
	anRGBehavior := RGClass named: #SomeClass parent: parent.
	self checkBasicPropertiesOf: anRGBehavior.
	self assert: anRGBehavior name equals: #SomeClass.
	self assert: anRGBehavior parent equals: parent.

]

{ #category : #tests }
RGClassDescripitonStrategyTest >> testCreationByParent [

	| anRGBehavior parent |
	
	parent := RGEnvironment	new.
	anRGBehavior := RGClass parent: parent.
	self checkBasicPropertiesOf: anRGBehavior.
	self assert: (anRGBehavior hasResolvedName) not.
	self assert: anRGBehavior parent equals: parent.
]

{ #category : #tests }
RGClassDescripitonStrategyTest >> testCreationByUnnamed [

	| anRGBehavior parent |
	
	anRGBehavior := RGClass parent: parent.
	self checkBasicPropertiesOf: anRGBehavior.
	self assert: (anRGBehavior hasResolvedName) not.

]

{ #category : #tests }
RGClassDescripitonStrategyTest >> testCreationByUnresolved [

	| anRGBehavior |
	
	anRGBehavior := RGClass unresolved.
	self checkBasicPropertiesOf: anRGBehavior.
	self assert: (anRGBehavior hasResolvedName) not.
	self assert: anRGBehavior isRingResolved not.

]

{ #category : #tests }
RGClassDescripitonStrategyTest >> testCreationByUnresolvedNamed [

	| anRGBehavior parent |
	
	parent := RGEnvironment	new.
	anRGBehavior := RGClass unresolvedNamed: #SomeClass withParent: parent.
	self checkBasicPropertiesOf: anRGBehavior.
	self assert: anRGBehavior name equals: #SomeClass.
	self assert: anRGBehavior parent equals: parent.
	self assert: anRGBehavior isRingResolved not.
	
]

{ #category : #tests }
RGClassDescripitonStrategyTest >> testDefaultLayout [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newClass.
	self assert: anRGBehavior defaultLayout isPointerLayout 
]

{ #category : #tests }
RGClassDescripitonStrategyTest >> testDefaultMetaclass [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newClass.
	self assert: anRGBehavior defaultMetaClass isMeta 
]

{ #category : #tests }
RGClassDescripitonStrategyTest >> testEmtpyClassVariableString [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newClass.
	self assert: anRGBehavior classVariablesString equals: ''.

]

{ #category : #tests }
RGClassDescripitonStrategyTest >> testInstSize [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newClass.
	
	anRGBehavior instanceVariables: #(a b c).
	
	self assert: anRGBehavior instSize equals: 3.
]

{ #category : #tests }
RGClassDescripitonStrategyTest >> testInstanceVariablesString [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newClass.
	
	anRGBehavior instanceVariables: #(a b c).
	
	self assert: anRGBehavior instanceVariablesString equals: 'a b c'.
]

{ #category : #tests }
RGClassDescripitonStrategyTest >> testKindOfSubclass [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newClass.
	
	self assert: anRGBehavior kindOfSubclass equals: ' subclass: '.
]

{ #category : #tests }
RGClassDescripitonStrategyTest >> testLayout [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newClass.
	self assert: anRGBehavior layout isPointerLayout 
]

{ #category : #tests }
RGClassDescripitonStrategyTest >> testLayoutSet [

	| anRGBehavior layout |
	
	anRGBehavior := RGBehavior newClass.
	layout := RGCompiledMethodLayout parent: anRGBehavior.
	anRGBehavior layout: layout.
	self assert: anRGBehavior layout equals: layout.
]

{ #category : #tests }
RGClassDescripitonStrategyTest >> testSetInstanceVariables [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newClass.
	
	anRGBehavior instanceVariables: #(a b c).
	
	self assert: anRGBehavior instVarNames equals: #(a b c).
]

{ #category : #tests }
RGClassDescripitonStrategyTest >> testSharedPools [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newClass.
	
	self assert: anRGBehavior sharedPools isEmpty.
]

{ #category : #tests }
RGClassDescripitonStrategyTest >> testUnresolvedClassGroupCreatorProperties [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newClass.
	self assert: (anRGBehavior superclass propertyNamed: #creator) equals: anRGBehavior.
	self assert: (anRGBehavior metaclass propertyNamed: #creator) equals: anRGBehavior.
	self assert: (anRGBehavior metaclass superclass propertyNamed: #creator) equals: anRGBehavior.

]

{ #category : #tests }
RGClassDescripitonStrategyTest >> testUnresolvedClassGroupRoleProperties [

	| anRGBehavior |
	
	anRGBehavior := RGBehavior newClass.
	self assert: (anRGBehavior superclass propertyNamed: #role) equals: #superclass.
	self assert: (anRGBehavior metaclass propertyNamed: #role) equals: #metaclass.
	self assert: (anRGBehavior metaclass superclass propertyNamed: #role) equals: #superclassMetaclass.

]
