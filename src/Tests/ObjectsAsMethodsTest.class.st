Class {
	#name : #ObjectsAsMethodsTest,
	#superclass : #TestCase,
	#category : #'Tests-ObjectsAsMethods'
}

{ #category : #running }
ObjectsAsMethodsTest >> setUp [
	super setUp.
	SystemAnnouncer uniqueInstance suspendAllWhile: [
		self class addSelector: #add:with: withMethod: ObjectsAsMethodsExample new.
		self class addSelector: #answer42 withMethod: ObjectsAsMethodsExample new.
		self class addSelector: #foo withMethod: AbstractObjectsAsMethod new ]
]

{ #category : #running }
ObjectsAsMethodsTest >> tearDown [
	SystemAnnouncer uniqueInstance suspendAllWhile: [
		self class removeSelector: #add:with:.
		self class removeSelector: #answer42.
		self class removeSelector: #foo ].
	super tearDown 
]

{ #category : #testing }
ObjectsAsMethodsTest >> testAddNumbers [
	self assert: (self add: 3 with: 4) equals: 7.
	self assert: (self perform: #add:with: withArguments: #(3 4)) equals: 7
]

{ #category : #testing }
ObjectsAsMethodsTest >> testAnswer42 [
	self assert: self answer42 equals: 42
]

{ #category : #testing }
ObjectsAsMethodsTest >> testDNU [ 

	self should: [self foo] raise: MessageNotUnderstood
]
