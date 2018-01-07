Trait {
	#name : #TRG2BehaviorDefinitionTest,
	#category : #'Ring2-Tests-Core'
}

{ #category : #tests }
TRG2BehaviorDefinitionTest >> testBehavior [
	| newBehavior method |
	newBehavior := self behaviorClass named: #SomeClass.
	self deny: (newBehavior includesSelector: #method).
	method := RG2MethodDefinition named: #method parent: newBehavior.
	newBehavior addLocalMethod: method.
	self deny: method isBehavior.
	self assert: (newBehavior includesSelector: #method).
	self assert: (newBehavior allInstVarNames isEmpty).
	self assert: (newBehavior allSlots isEmpty).
	self assert: (newBehavior bindingOf: #someVariable) isNil.
	self assert: (newBehavior classVariablesBindings isEmpty).
	
	self deny: (newBehavior isRing2FullyUnresolved).

	self deny: newBehavior hasTraitComposition.	
	

]

{ #category : #tests }
TRG2BehaviorDefinitionTest >> testBehaviorWithCategorizedMethods [
	| newBehavior method |
	newBehavior := self behaviorClass named: #SomeClass.
	self assert: newBehavior name equals: #SomeClass.
	self
		assert: (newBehavior hasUnresolvedAll: #(localMethods protocols)).
	newBehavior isTrait
		ifFalse: [ self assert: (newBehavior hasUnresolved: #superclass) ].
	self assert: newBehavior ask methods isEmpty.
	self assert: newBehavior ask protocols isEmpty.
	method := RG2MethodDefinition named: #someMethod parent: newBehavior.
	newBehavior addLocalMethod: method.
	self assert: (newBehavior hasResolved: #tagsForMethods).

]

{ #category : #tests }
TRG2BehaviorDefinitionTest >> testBehaviorWithMethods [
	| newBehavior |
	newBehavior := self behaviorClass named: #SomeClass.
	self assert: newBehavior name equals: #SomeClass.
	self
		assert: (newBehavior hasUnresolvedAll: #(localMethods protocols)).
	newBehavior isTrait
		ifFalse: [ self assert: (newBehavior hasUnresolved: #superclass) ].
	self assert: newBehavior localMethods isEmpty.
	1 to: 10 do: [ :i | 
		| m |
		m := RG2MethodDefinition
			named: 'method' , i asString
			parent: newBehavior.
		newBehavior addLocalMethod: m ].
	self assert: (newBehavior hasResolved: #localMethods).
	self
		assert:
			(newBehavior localMethods
				allSatisfy: [ :each | each environment = newBehavior environment ]).
	self assert: newBehavior localMethods size equals: 10.
	(newBehavior localMethods first: 4)
		do: [ :each | newBehavior removeLocalMethod: each ].
	self assert: newBehavior localMethods size equals: 6.
	newBehavior cleanLocalMethods.
	self assert: newBehavior localMethods isEmpty
]

{ #category : #tests }
TRG2BehaviorDefinitionTest >> testBehaviorWithOneMethod [
	| newBehavior method |
	newBehavior := self behaviorClass named: #SomeClass.
	self assert: newBehavior name equals: #SomeClass.
	self
		assert: (newBehavior hasUnresolvedAll: #(localMethods protocols)).
	newBehavior isTrait
		ifFalse: [ self assert: (newBehavior hasUnresolved: #superclass) ].
	self assert: newBehavior localMethods isEmpty.
	method := RG2MethodDefinition named: #someMethod parent: newBehavior.
	self assert: method environment == newBehavior environment.
	self assert: newBehavior localMethods isEmpty.	"we do not add the method to the behavior"
	newBehavior := self behaviorClass named: #SomeClass.
	newBehavior cleanLocalMethods.
	self assert: (newBehavior hasResolved: #localMethods).
	self assert: newBehavior localMethods isEmpty.
	method := RG2MethodDefinition named: #someMethod parent: newBehavior.
	newBehavior addLocalMethod: method.
	self assert: method environment == newBehavior environment.
	self assert: (newBehavior hasResolved: #localMethods).
	self assert: newBehavior localMethods anyOne name equals: #someMethod.
	newBehavior removeLocalMethod: method.
	self assert: newBehavior localMethods isEmpty.
	self assert: (newBehavior hasResolved: #localMethods)
]

{ #category : #tests }
TRG2BehaviorDefinitionTest >> testBehaviorWithProtocols [
	| newBehavior |
	newBehavior := self behaviorClass named: #SomeClass.
	self assert: newBehavior name equals: #SomeClass.
	self
		assert: (newBehavior hasUnresolvedAll: #(localMethods tagsForMethods)).
	newBehavior isTrait
		ifFalse: [ self assert: (newBehavior hasUnresolved: #superclass) ].
	self assert: newBehavior methods isEmpty.
	1 to: 10 do: [ :i | 
		newBehavior addProtocol: ('protocol' , i asString) asSymbol ].
	self assert: (newBehavior hasResolved: #tagsForMethods).
	self assert: (newBehavior protocols allSatisfy: #isSymbol).
	self assert: newBehavior protocols size equals: 10.
	(newBehavior protocols first: 4)
		do: [ :each | newBehavior removeProtocol: each ].
	self assert: newBehavior protocols size equals: 6.
	newBehavior cleanProtocols.
	self assert: newBehavior protocols isEmpty
]

{ #category : #tests }
TRG2BehaviorDefinitionTest >> testBehaviorWithUnategorizedMethod [
	| newBehavior method |
	
	newBehavior := self behaviorClass named: #SomeClass.
	self assert: newBehavior name equals: #SomeClass.
	self assert: (newBehavior hasUnresolvedAll: #(localMethods protocols)).
	newBehavior isTrait ifFalse: [  
		self assert: (newBehavior hasUnresolved: #superclass).].
	self assert: (newBehavior methods isEmpty).
	self assert: (newBehavior protocols isEmpty).

	method := RG2MethodDefinition named: #someMethod parent: newBehavior. 
	newBehavior addLocalMethod: method.
	
	self assert: (newBehavior hasResolved: #tagsForMethods).
	self assert: newBehavior protocols size equals: 1.
	self assert: newBehavior protocols first equals: 'as yet unclassified'.
	newBehavior cleanProtocols.

	self assert: newBehavior protocols size equals: 1.
	self assert: newBehavior protocols first equals: 'as yet unclassified'.
	
	newBehavior cleanLocalMethods.
	self assert: (newBehavior protocols isEmpty).
	

]

{ #category : #tests }
TRG2BehaviorDefinitionTest >> testBehaviorWithUnategorizedMethods [
	| newBehavior method1 method2 |
	newBehavior := self behaviorClass named: #SomeClass.
	self assert: newBehavior name equals: #SomeClass.
	self
		assert: (newBehavior hasUnresolvedAll: #(localMethods tagsForMethods)).
	newBehavior isTrait
		ifFalse: [ self assert: (newBehavior hasUnresolved: #superclass) ].
	self assert: newBehavior methods isEmpty.
	self assert: newBehavior protocols isEmpty.
	method1 := RG2MethodDefinition named: #method1 parent: newBehavior.
	method2 := RG2MethodDefinition named: #method2 parent: newBehavior.
	newBehavior addLocalMethod: method1.
	newBehavior addLocalMethod: method2.
	self assert: (newBehavior hasResolved: #tagsForMethods).
	self assert: newBehavior protocols size equals: 1.
	self assert: newBehavior protocols first equals: 'as yet unclassified'.
	newBehavior cleanProtocols.
	self assert: newBehavior protocols size equals: 1.
	self assert: newBehavior protocols first equals: 'as yet unclassified'.
	newBehavior cleanLocalMethods.
	self assert: newBehavior protocols isEmpty
]

{ #category : #tests }
TRG2BehaviorDefinitionTest >> testCopyForBehaviorDefinition [
	
	| env aClass copy |
	
	env := RG2Environment new.
	aClass := env ensureClassNamed: #SomeClass.
	aClass superclass: (env ensureClassNamed: #SomeSuperclass).
	aClass tagWith: #tag1.
	aClass tagWith: #tag2.
		
	copy := aClass copyForBehaviorDefinition.
	
	self deny: copy == aClass. 
	self deny: copy superclass == aClass superclass. 
	self assert: copy superclass name equals: #SomeSuperclass.
	self assert: copy tags sorted equals: #(tag1 tag2).
]

{ #category : #tests }
TRG2BehaviorDefinitionTest >> testHasMethods [
	| newBehavior |
	newBehavior := self behaviorClass named: #SomeClass.
	self deny: newBehavior hasMethods.
	1 to: 10 do: [ :i | 
		| m |
		m := RG2MethodDefinition
			named: 'method' , i asString
			parent: newBehavior.
		newBehavior addLocalMethod: m ].
	self assert: newBehavior hasMethods.	
	newBehavior cleanLocalMethods.
	self deny: newBehavior hasMethods.

]

{ #category : #tests }
TRG2BehaviorDefinitionTest >> testIsBehavior [

	| newBehavior |
	newBehavior := self behaviorClass unnamed.
	
	self assert: newBehavior isBehavior equals: newBehavior isTrait not.

	
]

{ #category : #tests }
TRG2BehaviorDefinitionTest >> testMakeReslolvedResolvesEverything [

	| behavior |

	behavior := self behaviorClass unnamed.
	behavior makeResolved.
	"TODO: FIX!"
"	self assert: behavior unresolvedProperties isEmpty."
]

{ #category : #tests }
TRG2BehaviorDefinitionTest >> testNewBehavior [
	| newBehavior newSuperclass |
	newBehavior := self behaviorClass unnamed.
	self assert: newBehavior isRing2Resolved.
	self
		assert: (newBehavior hasUnresolvedAll: #(localMethods protocols)).
	newBehavior isTrait
		ifFalse: [ self assert: (newBehavior hasUnresolved: #superclass) ].
	newBehavior name: #SomeClass.
	self assert: newBehavior name equals: #SomeClass.
	self assert: (newBehavior hasResolved: #name).
	self deny: (newBehavior hasUnresolved: #name).
	newBehavior isTrait
		ifFalse: [ self assert: newBehavior superclass isRing2Resolved not ].
	"	self assert: (newBehavior superclass environment == newBehavior environment).
"
	newSuperclass := RG2BehaviorDefinition
		named: #MySuperclass
		parent: newBehavior environment.
	self assert: newSuperclass environment == newBehavior environment.
	self
		assert: (newSuperclass hasUnresolvedAll: #(superclass localMethods protocols))
]

{ #category : #tests }
TRG2BehaviorDefinitionTest >> testProtocols [

	| newBehavior newSuperclass |
	newBehavior := self behaviorClass unnamed.
	
	self assert: newBehavior asYetUnclassifiedProtocolName isSymbol.

	
]
