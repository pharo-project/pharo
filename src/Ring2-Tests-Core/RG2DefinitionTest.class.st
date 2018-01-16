Class {
	#name : #RG2DefinitionTest,
	#superclass : #RG2Test,
	#category : #'Ring2-Tests-Core'
}

{ #category : #'as yet unclassified' }
RG2DefinitionTest >> testMerge [

	| def1 def2 | 
	
	"we cannot test RG2Definition directly here"
	def1 := RG2BehaviorDefinition named: #SomeDefinition.
	def2 := RG2BehaviorDefinition unnamed.
	def1 merge: def2.
	self assert: (def1 hasResolved: #name).
	self assert: def1 name equals: #SomeDefinition.
	
	def1 := RG2BehaviorDefinition unnamed.
	def2 := RG2BehaviorDefinition named: #SomeDefinition.
	def1 merge: def2.
	self assert: (def1 hasResolved: #name).
	self assert: def1 name equals: #SomeDefinition.
	
	def1 := RG2BehaviorDefinition unnamed.
	def2 := RG2BehaviorDefinition unnamed.
	def1 merge: def2.
	self assert: (def1 hasUnresolved: #name).
	
	def1 := RG2BehaviorDefinition named: #SomeDefinition.
	def2 := RG2BehaviorDefinition named: #SomeDefinition.
	def1 merge: def2.
	self assert: (def1 hasResolved: #name).
	self assert: def1 name equals: #SomeDefinition.

	def1 := RG2BehaviorDefinition named: #SomeDefinition1.
	def2 := RG2BehaviorDefinition named: #SomeDefinition2.
	self should: [def1 merge: def2] raise: RG2MergeError.
	
]

{ #category : #'as yet unclassified' }
RG2DefinitionTest >> testMiscellaneous [

	| def fullCopy| 
	
	"we cannot test RG2Definition directly here"
	def := RG2BehaviorDefinition named: #SomeDefinition.
	self assert: (def asRing2Definition == def).
	self assert: def fullName equals: #SomeDefinition.
	
	fullCopy := def fullCopy.
	self assert: (fullCopy ~~ def).
	self assert: fullCopy name equals: def name.
	
	self deny: (def unresolvedValue isRing2Resolved).
	
	
	
]

{ #category : #'as yet unclassified' }
RG2DefinitionTest >> testParent [

	| def1 def2 | 
	
	def1 := RG2Definition new.
	def2 := RG2Definition new.
	def2 pvtParent: def1.
	self assert: (def2 parent == def1).
	

]

{ #category : #'as yet unclassified' }
RG2DefinitionTest >> testProperties [

	| def | 
	
	def := RG2Definition new.
	
	self assert: def hasProperties not.
	self assert: (def hasPropertyNamed: #note) not.
	self assert: def propertyNames isEmpty.
	self assert: (def propertyNamed: #note ifAbsent: [#someNote]) equals: #someNote.
	self assert: (def hasPropertyNamed: #note) not.
	self assert: (def propertyNamed: #note ifAbsentPut: [#someNote]) equals: #someNote.
	self assert: (def hasPropertyNamed: #note).
	self assert: (def propertyNamed: #note) equals: #someNote.
	self assert: def hasProperties.
	self assert: def propertyNames asArray equals: {#note}.
	def propertyNamed: #note2 put: #someNote2.
	self assert: (def propertyNames asArray includesAll: {#note. #note2}).
	def removePropertyNamed: #note.
	self assert: def propertyNames asArray equals: {#note2}.
	def removePropertyNamed: #note2.
	self assert: def hasProperties not.
	self assert: (def hasPropertyNamed: #note) not.
	self assert: def propertyNames isEmpty.
	

]

{ #category : #tests }
RG2DefinitionTest >> testResolveability [

	"test if we can recolve standard objects"
	
	{ Object. 42. 'aString'. #aSymbol } do: [ :each |
		self deny: each isRing2Object.
		self assert: each isRing2FullyResolved.
		self deny: each isRing2FullyUnresolved  ]
	
]

{ #category : #tests }
RG2DefinitionTest >> testTypes [

	| def | 
	
	def := RG2Definition new.
	
	self deny: def isComment.
	self deny: def isEnvironment.
	self deny: def isGlobalVariable.
	self deny: def isLayout.
	self deny: def isMethod.
	self deny: def isPackage.
	self deny: def isSlot.
	self deny: def isTraitComposition.
	self deny: def isTraitTransformation.
	self deny: def isVariable.
	self deny: def isSlot.
	self deny: def isClass.
	self deny: def isMetaclass.
	self deny: def isMetaclassTrait.
	self deny: def isProtocol.
	self deny: def isTraitAlias.
	self deny: def isTraitComposition.
	self deny: def isTraitExclusion.
	self deny: def isTraitTransformation.
	self deny: def isVariableDefinition.

]
