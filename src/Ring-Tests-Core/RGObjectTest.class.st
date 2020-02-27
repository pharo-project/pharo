Class {
	#name : #RGObjectTest,
	#superclass : #RGTest,
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
RGObjectTest >> testFullCopy [

	| def fullCopy | 
	
	"we cannot test RGObject directly here"
	def := RGClass named: #SomeDefinition.
	
	fullCopy := def fullCopy.
	self assert: (fullCopy ~~ def).
	self assert: fullCopy name equals: def name.
	
	
]

{ #category : #tests }
RGObjectTest >> testNameAccess [
	| def |
	"we cannot test RGObject directly here"
	def := RGClass named: #SomeDefinition.
	self assert: def asRGDefinition identicalTo: def.
	self assert: def name equals: #SomeDefinition.
	self assert: def fullName equals: #SomeDefinition.
	def name: #NewName.
	self assert: def name equals: #NewName.
	self assert: def fullName equals: #NewName
]

{ #category : #tests }
RGObjectTest >> testParent [
	"test link to parent object"

	| def1 def2 |
	def1 := RGObject new.
	def2 := RGObject new.
	def2 pvtParent: def1.
	self assert: def2 parent identicalTo: def1
]

{ #category : #tests }
RGObjectTest >> testProperties [

	"test all method that access properties of Ring objects"
	
	| def | 
	
	def := RGObject new.
	
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
	self shouldnt: [def removePropertyNamed: #nonexisting] raise: KeyNotFound.
	self assert: def propertyNames asArray equals: {#note2}.
	def removePropertyNamed: #note2.
	self assert: def hasProperties not.
	self assert: (def hasPropertyNamed: #note) not.
	self assert: def propertyNames isEmpty.
	

]

{ #category : #tests }
RGObjectTest >> testResolveability [

	"test if we can resolve standard objects. They should be all resolved"
	
	{ Object. 42. 'aString'. #aSymbol } do: [ :each |
		self deny: each isRGObject.
		self assert: each isRingFullyResolved.
		self deny: each isRingFullyUnresolved  ]
	
]

{ #category : #tests }
RGObjectTest >> testResolvedNameOrNil [

	"should return nil for unresolved names"

	| def | 
	
	def := RGClass unresolved.
	self assert: def resolvedNameOrNil equals: nil.
	
	def := RGClass named: #SomeName.
	self assert: def resolvedNameOrNil equals: #SomeName.
		
]

{ #category : #tests }
RGObjectTest >> testTrivialMethods [

	| def | 
	
	def := RGObject named: #SomeDefinition.

	self deny: (def unresolvedValue isRingResolved).
	
	
	
]

{ #category : #tests }
RGObjectTest >> testTypes [

	"check all testing methods"

	| def | 
	
	def := RGObject new.
	
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

{ #category : #tests }
RGObjectTest >> testUnresolveName [

	"The name of any Ring object should be unresolveable"

	| def result | 
	
	"try to unresolve already unresolved"
	def := RGComment unresolved.
	self deny: (def hasResolvedName).
	def unresolveName.
	self deny: (def hasResolvedName).

	"unresolve existing name"
	def name: #SomeName.
	self assert: (def hasResolvedName).
	result := def unresolveName.
	self deny: (def hasResolvedName).
	self assert: result isRGUnresolvedValue.
]

{ #category : #tests }
RGObjectTest >> testUnresolvedValue [

	| def | 
	
	def := RGObject named: #SomeDefinition.

	self deny: (def unresolvedValue isRingResolved).
	
	
	
]
