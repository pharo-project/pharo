Class {
	#name : 'DeepCopierTest',
	#superclass : 'TestCase',
	#category : 'DeepCopier',
	#package : 'DeepCopier'
}

{ #category : 'tests' }
DeepCopierTest >> testDeepCopyDoubleParentReference [

	| parent child1 child2 copy |
	parent := DeepCopierTestData new.
	child1 := DeepCopierTestData new.
	child2 := DeepCopierTestData new.
	
	parent refer: { child1 . child2 }.
	child1 refer: parent. 
	child2 refer: parent. 
	
	copy := parent veryDeepCopy.
	
	"Created a new collection"
	self assert: copy refer ~= parent refer.
	self assert: copy refer first class equals: DeepCopierTestData.
	self assert: copy refer size equals: 2.
	self assert: copy refer first class equals: parent refer first class.
	
	self assert: copy refer first refer identicalTo: copy.
	self assert: copy refer second refer identicalTo: copy.  
]

{ #category : 'tests' }
DeepCopierTest >> testDeepCopyOnPoint [
	"by default do nothing but let us get a test"

	| point copy |
	point := 100@200.
	copy := point veryDeepCopy.
	self assert: point x equals: copy x.
	self assert: point y equals: copy y.
]

{ #category : 'tests' }
DeepCopierTest >> testDeepCopyOnSimpleCycle [

	| cyclic copy |
	cyclic := DeepCopierTestData aPointsToBThatPointsToA.
	copy := cyclic veryDeepCopy.
	self assert: copy refer ~= cyclic refer.
	self assert: copy refer class equals: DeepCopierTestData.
	self assert: copy refer class equals: cyclic refer class.
	
	
	self assert: copy refer refer ~= cyclic refer refer.
	self assert: copy refer refer class equals: DeepCopierTestData.
	self assert: copy refer refer class equals: cyclic refer refer class.
	
	self assert: copy refer refer identicalTo: copy
	
]

{ #category : 'tests' }
DeepCopierTest >> testDeepCopyOnTripleCycle [

	| cyclic copy |
	cyclic := DeepCopierTestData tripleCircle.
	copy := cyclic veryDeepCopy.
	
	self assert: copy refer ~= cyclic refer.
	self assert: copy refer class equals: DeepCopierTestData.
	self assert: copy refer class equals: cyclic refer class.
	
	
	self assert: copy refer refer ~= cyclic refer refer.
	self assert: copy refer refer class equals: DeepCopierTestData.
	self assert: copy refer refer class equals: cyclic refer refer class.
	
	self assert: copy refer refer refer ~= cyclic refer refer refer.
	self assert: copy refer refer refer class equals: DeepCopierTestData.
	self assert: copy refer refer refer class equals: cyclic refer refer refer class.
	
	self assert: copy refer refer refer identicalTo: copy
	
]
