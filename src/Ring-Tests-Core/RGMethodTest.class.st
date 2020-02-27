Class {
	#name : #RGMethodTest,
	#superclass : #RGTest,
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
RGMethodTest >> testAST [
	
	| method ast |

	method := RGMethod named: #someMethod.
	ast := method ast.
	self assert: ast isMethod.
	self assert: ast selector equals: #someMethod.
	

	
	
]

{ #category : #tests }
RGMethodTest >> testFullName [
	
	| method |

	method := RGMethod named: #someMethod.
	method parent: (method environment ensureClassNamed: #SomeClass).
	self assert: method fullName equals: 'SomeClass>>#someMethod'.

	method parent metaclass makeResolved.
	method parent: method parent metaclass.
	self assert: method fullName equals: 'SomeClass class>>#someMethod'.	
]

{ #category : #tests }
RGMethodTest >> testImplicitEnvironment [
	
	| def def2 env |

	def := RGMethod unnamed.
	env := def environment.

	self checkImplicitSingleClassEnvironmentOf: def parent.	
	self assert: def parent localMethods size equals: 1.
	self assert: def parent protocols size equals: 1.
	self assert: (def parent hasResolved: #tagsForMethods).
	self assert: def parent tagsForMethods isEmpty.
	self assert: def parent protocols size equals: 1.
	self assert: def parent protocols anyOne equals: 'as yet unclassified'.

	self assert: (def isRingResolved).
	
	def2 := RGMethod named: #someMethod parent: def parent.
	def2 parent addLocalMethod: def2.
	self assert: def parent localSelectors sorted equals: #(someMethod unresolved).
	self checkImplicitSingleClassEnvironmentOf: def parent.	

	self assert: def parent protocols size equals: 1.
	self assert: def2 parent localMethods size equals: 2.

]

{ #category : #tests }
RGMethodTest >> testMetaSide [

	| anEnvironemnt aMethod |
	
	anEnvironemnt := RGEnvironment new.
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
RGMethodTest >> testNewMethod [
	| method time |
	
	method := RGMethod unnamed.
	self assert: method isMethod. 
	
	self assert: (method isRingResolved).
	
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
RGMethodTest >> testNewNamedMethod [

	| method |
	
	method := RGMethod named: #someMessage.
	
	self assert: (method isRingResolved).
	
	self assert: (method hasResolvedName).
	self assert: (method hasUnresolved: #sourceCode).

	self assert: method selector equals: 'someMessage'.
	self assert: method sourceCode lines first equals: 'someMessage'.

	self deny: method isMetaSide.

]

{ #category : #tests }
RGMethodTest >> testPackages [
	| method class |
	" implicit environment "
	method := RGMethod unnamed.

	self assert: method parent package identicalTo: method package.
	self assert: method environment ask packages size equals: 2.

	" adding to existing class definition "
	class := RGClass unnamed.
	method := RGMethod parent: class.
	class addLocalMethod: method.

	self assert: method parent package identicalTo: method package.
	self assert: method environment ask packages size equals: 2.

	" adding to existing class definition with package "
	class := RGClass unnamed.
	method := RGMethod parent: class.
	class addLocalMethod: method.
	method package: (RGPackage named: 'SomePackage' parent: method environment).

	self assert: method parent package ~~ method package.
	self assert: method environment ask packages size equals: 3
]

{ #category : #tests }
RGMethodTest >> testRemovePackage [
	| method class |
	method := RGMethod unnamed.
	class := method parent.

	method package: (RGPackage named: 'SomePackage' parent: method environment).

	self assert: method parent package ~~ method package.
	self assert: method environment ask packages size equals: 3.

	method package: method parent package.	"return it"

	self assert: method parent package identicalTo: method package.
	self assert: method environment ask packages size equals: 3.

	method environment removeUnusedPackages.
	self assert: method environment ask packages size equals: 2
]

{ #category : #tests }
RGMethodTest >> testResolvingConsistency [

	| anObject |
	anObject := RGMethod unnamed.
	self checkClassesConsistency: anObject and: anObject makeResolved
]

{ #category : #tests }
RGMethodTest >> testSourceCode [
	
	| method source |

	method := RGMethod named: #someMethod.
	self assert: method hasSourceCode.
	source := 'someMethod ^ self'.
	method sourceCode: source.
	self assert: method hasSourceCode.
	self assert: method sourceCode equals: source.
	self assert: method selector equals: #someMethod.
	self assert: method numArgs equals: 0.
	
	method := RGMethod named: #+.
	self assert: method numArgs equals: 1.
	method := RGMethod named: #someMethod:.
	self assert: method numArgs equals: 1.
	method := RGMethod named: #someMethod:arg:.
	self assert: method numArgs equals: 2.

	
	
]

{ #category : #tests }
RGMethodTest >> testTagsCollection [
	
	| method env tag1 tag2 tag3 |

	method := RGMethod unnamed.
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
