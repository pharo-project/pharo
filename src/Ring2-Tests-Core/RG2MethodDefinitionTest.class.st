Class {
	#name : #RG2MethodDefinitionTest,
	#superclass : #RG2Test,
	#category : #'Ring2-Tests-Core'
}

{ #category : #tests }
RG2MethodDefinitionTest >> testAST [
	
	| method ast |

	method := RG2MethodDefinition named: #someMethod.
	ast := method ast.
	self assert: ast isMethod.
	self assert: ast selector equals: #someMethod.
	

	
	
]

{ #category : #tests }
RG2MethodDefinitionTest >> testFullName [
	
	| method |

	method := RG2MethodDefinition named: #someMethod.
	method parent: (method environment ensureClassNamed: #SomeClass).
	self assert: method fullName equals: 'SomeClass>>#someMethod'.

	method parent metaclass makeResolved.
	method parent: method parent metaclass.
	self assert: method fullName equals: 'SomeClass class>>#someMethod'.	
]

{ #category : #tests }
RG2MethodDefinitionTest >> testImplicitEnvironment [
	
	| def def2 env |

	def := RG2MethodDefinition unnamed.
	env := def environment.

	self checkImplicitSingleClassEnvironmentOf: def parent.	
	self assert: def parent localMethods size equals: 1.
	self assert: def parent protocols size equals: 1.
	self assert: (def parent hasResolved: #tagsForMethods).
	self assert: def parent tagsForMethods isEmpty.
	self assert: def parent protocols size equals: 1.
	self assert: def parent protocols anyOne equals: 'as yet unclassified'.

	self assert: (def isRing2Resolved).
	
	def2 := RG2MethodDefinition named: #someMethod parent: def parent.
	def2 parent addLocalMethod: def2.
	self assert: def parent localSelectors sorted equals: #(someMethod unresolved).
	self checkImplicitSingleClassEnvironmentOf: def parent.	

	self assert: def parent protocols size equals: 1.
	self assert: def2 parent localMethods size equals: 2.

]

{ #category : #tests }
RG2MethodDefinitionTest >> testMetaSide [

	| anEnvironemnt aMethod |
	
	anEnvironemnt := RG2Environment new.
	aMethod := (anEnvironemnt ensureClassNamed: 'SomeClass class') ensureLocalMethodNamed: #someMethod.
	self assert: aMethod isMetaSide.
	self deny: aMethod isFromTrait.
	aMethod := (anEnvironemnt ensureClassNamed: 'SomeClass') ensureLocalMethodNamed: #someMethod.
	self deny: aMethod isMetaSide.
	self deny: aMethod isFromTrait.
	aMethod := (anEnvironemnt ensureTraitNamed: 'SomeTrait classTrait') ensureLocalMethodNamed: #someMethod.
	self assert: aMethod isMetaSide.
	self assert: aMethod isFromTrait.
	aMethod := (anEnvironemnt ensureTraitNamed: 'SomeTrait') ensureLocalMethodNamed: #someMethod.
	self deny: aMethod isMetaSide.
	self assert: aMethod isFromTrait.

	
]

{ #category : #tests }
RG2MethodDefinitionTest >> testNewMethod [
	| method time |
	
	method := RG2MethodDefinition unnamed.
	self assert: method isMethod. 
	
	self assert: (method isRing2Resolved).
	
	self assert: (method hasUnresolvedAll: #(name protocol sourceCode package author time)).


	self assert: (method hasUnresolvedAll: #(name sourceCode package author time)).
	self assert: method sourceCode lines first trimBoth equals: 'unresolvedMessage'.
	method sourceCode: 'someMessage ^ yourself'.
	self assert: (method hasUnresolvedAll: #(name package author time)).
	self assert: method sourceCode equals: 'someMessage ^ yourself'.

	self assert: method author equals: ''.
	method author: 'FullAuthorName'.
	self assert: (method hasUnresolvedAll: #(time name)).
	self assert: method author equals: 'FullAuthorName'.
	
	time := DateAndTime now.
	self assert: method time equals: DateAndTime new.
	method time: time.
	self assert: (method hasUnresolved: #name).
	self assert: method time equals: time.

]

{ #category : #tests }
RG2MethodDefinitionTest >> testNewNamedMethod [

	| method |
	
	method := RG2MethodDefinition named: #someMessage.
	
	self assert: (method isRing2Resolved).
	
	self assert: (method hasResolved: #name).
	self assert: (method hasUnresolved: #sourceCode).

	self assert: method selector equals: 'someMessage'.
	self assert: method sourceCode lines first equals: 'someMessage'.

	self deny: method isMetaSide.

]

{ #category : #tests }
RG2MethodDefinitionTest >> testPackages [
	
	| method class |

	" implicit environment "
	method := RG2MethodDefinition unnamed.

	self assert: (method parent package == method package).
	self assert: method environment ask packages size equals: 2.
	
	" adding to existing class definition "
	class := RG2ClassDefinition unnamed.
	method := RG2MethodDefinition parent: class.
	class addLocalMethod: method.
	
	self assert: (method parent package == method package).
	self assert: method environment ask packages size equals: 2.

	" adding to existing class definition with package "
	class := RG2ClassDefinition unnamed.
	method := RG2MethodDefinition parent: class.
	class addLocalMethod: method.
	method package: (RG2PackageDefinition named: 'SomePackage' parent: method environment).
	
	self assert: (method parent package ~~ method package).
	self assert: method environment ask packages size equals: 3.

	
]

{ #category : #tests }
RG2MethodDefinitionTest >> testRemovePackage [
	
	| method class |

	method := RG2MethodDefinition unnamed.
	class := method parent.
	
	method package: (RG2PackageDefinition named: 'SomePackage' parent: method environment).
	
	self assert: (method parent package ~~ method package).
	self assert: method environment ask packages size equals: 3.
	
	method package: method parent package. "return it"

	self assert: (method parent package == method package).
	self assert: method environment ask packages size equals: 3.
	
	method environment removeUnusedPackages.
	self assert: method environment ask packages size equals: 2.
	
	
]

{ #category : #tests }
RG2MethodDefinitionTest >> testSourceCode [
	
	| method source |

	method := RG2MethodDefinition named: #someMethod.
	self assert: method hasSourceCode.
	source := 'someMethod ^ self'.
	method sourceCode: source.
	self assert: method hasSourceCode.
	self assert: method sourceCode equals: source.
	self assert: method selector equals: #someMethod.
	self assert: method numArgs equals: 0.
	
	method := RG2MethodDefinition named: #+.
	self assert: method numArgs equals: 1.
	method := RG2MethodDefinition named: #someMethod:.
	self assert: method numArgs equals: 1.
	method := RG2MethodDefinition named: #someMethod:arg:.
	self assert: method numArgs equals: 2.

	
	
]

{ #category : #tests }
RG2MethodDefinitionTest >> testTagsCollection [
	
	| method env tag1 tag2 tag3 |

	method := RG2MethodDefinition unnamed.
	env := method environment.

	self assert: method tags isEmpty.
	self assert: (method hasUnresolved: #tags).
	
	tag1 := #tag1.
	method tagWith: #tag1.
	self assert: method tags size equals: 1.

	self assert: (method hasResolved: #tags).
	
	tag2 := #tag2.
	method tagWith: tag2.
	self assert: method tags size equals: 2.
	
	tag3 := #tag3.
	method tagWith: tag3.
	self assert: method tags size equals: 3.

	method untagFrom: tag3.
	self assert: method tags size equals: 2.

	method cleanTags.
	self assert: method tags isEmpty.
	self assert: (method hasResolved: #tags).

]
