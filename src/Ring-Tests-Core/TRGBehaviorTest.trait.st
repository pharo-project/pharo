Trait {
	#name : #TRGBehaviorTest,
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
TRGBehaviorTest >> testBehavior [
	| newBehavior method |
	newBehavior := self behaviorClass named: #SomeClass.
	self deny: (newBehavior includesSelector: #method).
	method := RGMethod named: #method parent: newBehavior.
	newBehavior addLocalMethod: method.
	self deny: method isBehavior.
	self assert: (newBehavior includesSelector: #method).
	self assert: (newBehavior allInstVarNames isEmpty).
	self assert: (newBehavior allSlots isEmpty).
	self assert: (newBehavior bindingOf: #someVariable) isNil.
	self assert: (newBehavior classVariablesBindings isEmpty).
	
	self deny: (newBehavior isRingFullyUnresolved).

	self deny: newBehavior hasTraitComposition.	
	

]

{ #category : #tests }
TRGBehaviorTest >> testBehaviorWithCategorizedMethods [
	| newBehavior method |
	newBehavior := self behaviorClass named: #SomeClass.
	self assert: newBehavior name equals: #SomeClass.
	self
		assert: (newBehavior hasUnresolvedAll: #(localMethods protocols)).
	newBehavior isTrait
		ifFalse: [ self assert: (newBehavior hasUnresolved: #superclass) ].
	self assert: newBehavior ask methods isEmpty.
	self assert: newBehavior ask protocols isEmpty.
	method := RGMethod named: #someMethod parent: newBehavior.
	newBehavior addLocalMethod: method.
	self assert: (newBehavior hasResolved: #tagsForMethods).

]

{ #category : #tests }
TRGBehaviorTest >> testBehaviorWithMethods [
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
		m := RGMethod
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
TRGBehaviorTest >> testBehaviorWithOneMethod [
	| newBehavior method |
	newBehavior := self behaviorClass named: #SomeClass.
	self assert: newBehavior name equals: #SomeClass.
	self
		assert: (newBehavior hasUnresolvedAll: #(localMethods protocols)).
	newBehavior isTrait
		ifFalse: [ self assert: (newBehavior hasUnresolved: #superclass) ].
	self assert: newBehavior localMethods isEmpty.
	method := RGMethod named: #someMethod parent: newBehavior.
	self assert: method environment == newBehavior environment.
	self assert: newBehavior localMethods isEmpty.	"we do not add the method to the behavior"
	newBehavior := self behaviorClass named: #SomeClass.
	newBehavior cleanLocalMethods.
	self assert: (newBehavior hasResolved: #localMethods).
	self assert: newBehavior localMethods isEmpty.
	method := RGMethod named: #someMethod parent: newBehavior.
	newBehavior addLocalMethod: method.
	self assert: method environment == newBehavior environment.
	self assert: (newBehavior hasResolved: #localMethods).
	self assert: newBehavior localMethods anyOne name equals: #someMethod.
	newBehavior removeLocalMethod: method.
	self assert: newBehavior localMethods isEmpty.
	self assert: (newBehavior hasResolved: #localMethods)
]

{ #category : #tests }
TRGBehaviorTest >> testBehaviorWithProtocols [
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
TRGBehaviorTest >> testBehaviorWithUnategorizedMethod [
	| newBehavior method |
	
	newBehavior := self behaviorClass named: #SomeClass.
	self assert: newBehavior name equals: #SomeClass.
	self assert: (newBehavior hasUnresolvedAll: #(localMethods protocols)).
	newBehavior isTrait ifFalse: [  
		self assert: (newBehavior hasUnresolved: #superclass).].
	self assert: (newBehavior methods isEmpty).
	self assert: (newBehavior protocols isEmpty).

	method := RGMethod named: #someMethod parent: newBehavior. 
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
TRGBehaviorTest >> testBehaviorWithUnategorizedMethods [
	| newBehavior method1 method2 |
	newBehavior := self behaviorClass named: #SomeClass.
	self assert: newBehavior name equals: #SomeClass.
	self
		assert: (newBehavior hasUnresolvedAll: #(localMethods tagsForMethods)).
	newBehavior isTrait
		ifFalse: [ self assert: (newBehavior hasUnresolved: #superclass) ].
	self assert: newBehavior methods isEmpty.
	self assert: newBehavior protocols isEmpty.
	method1 := RGMethod named: #method1 parent: newBehavior.
	method2 := RGMethod named: #method2 parent: newBehavior.
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
TRGBehaviorTest >> testCopyForBehaviorDefinition [
	
	| env aClass copy |
	
	env := RGEnvironment new.
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
TRGBehaviorTest >> testHasMethods [
	| newBehavior |
	newBehavior := self behaviorClass named: #SomeClass.
	self deny: newBehavior hasMethods.
	1 to: 10 do: [ :i | 
		| m |
		m := RGMethod
			named: 'method' , i asString
			parent: newBehavior.
		newBehavior addLocalMethod: m ].
	self assert: newBehavior hasMethods.	
	newBehavior cleanLocalMethods.
	self deny: newBehavior hasMethods.

]

{ #category : #tests }
TRGBehaviorTest >> testIsBehavior [

	| newBehavior |
	newBehavior := self behaviorClass unnamed.
	
	self assert: newBehavior isBehavior

	
]

{ #category : #tests }
TRGBehaviorTest >> testMakeReslolvedResolvesEverything [

	| behavior |

	behavior := self behaviorClass unnamed.
	behavior makeResolved.
	self assert: behavior unresolvedProperties isEmpty.
]

{ #category : #tests }
TRGBehaviorTest >> testNewBehavior [
	| newBehavior newSuperclass |
	newBehavior := self behaviorClass unnamed.
	self assert: newBehavior isRingResolved.
	self
		assert: (newBehavior hasUnresolvedAll: #(localMethods protocols)).
	newBehavior isTrait
		ifFalse: [ self assert: (newBehavior hasUnresolved: #superclass) ].
	newBehavior name: #SomeClass.
	self assert: newBehavior name equals: #SomeClass.
	self assert: (newBehavior hasResolvedName).
	self deny: (newBehavior hasUnresolved: #name).
	newBehavior isTrait
		ifFalse: [ self assert: newBehavior superclass isRingResolved not ].
	"	self assert: (newBehavior superclass environment == newBehavior environment).
"
	newSuperclass := RGClass
		named: #MySuperclass
		parent: newBehavior environment.
	self assert: newSuperclass environment == newBehavior environment.
	self
		assert: (newSuperclass hasUnresolvedAll: #(superclass localMethods protocols))
]

{ #category : #tests }
TRGBehaviorTest >> testProtocols [

	| newBehavior |
	newBehavior := self behaviorClass unnamed.
	
	self assert: newBehavior asYetUnclassifiedProtocolName isSymbol.

	
]
