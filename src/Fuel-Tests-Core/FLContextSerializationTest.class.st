"
SUnit tests for fuel serialization of methods contexts
"
Class {
	#name : #FLContextSerializationTest,
	#superclass : #FLSerializationTest,
	#instVars : [
		'thisContextSample'
	],
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #'closures for testing' }
FLContextSerializationTest class >> blockClosureContextTesting [
	^ [self class] asContext
]

{ #category : #'closures for testing' }
FLContextSerializationTest class >> blockClosureContextTestingWithSender: aSenderContext [
	^ [self class] asContextWithSender: aSenderContext
]

{ #category : #'closures for testing' }
FLContextSerializationTest class >> blockClosureContextWithOutPointerTesting [
	| string |
	string := 'test'.
	^ [self class. string asUppercase] asContext
]

{ #category : #running }
FLContextSerializationTest class >> thisContextSample [
	^ thisContext copy
]

{ #category : #running }
FLContextSerializationTest >> setUp [
	super setUp.
	thisContextSample := self class thisContextSample.
]

{ #category : #tests }
FLContextSerializationTest >> testContext [
	"This test should be improved"
	| context1  materializedContext1  |
	context1 := (Context newForMethod: FLPair >> #method1).
	context1 
		initializeWith: 23 
		stackPtr: 1 
		method: FLPair >> #method1 
		receiver:  (FLPair new right: 4; left: 2) 
		sender: nil.
		
	materializedContext1 := self resultOfSerializeAndMaterialize: context1.
	context1 assertWellMaterializedInto: materializedContext1 in: self.
]

{ #category : #tests }
FLContextSerializationTest >> testContextThisContext [
	| materializedContext context |
	context := thisContextSample.
	materializedContext := self resultOfSerializeAndMaterialize: context.

	self deny: context identicalTo: materializedContext.
	"I cannot compare by = because MethodContext do not implement it and it will finally use the identity."
	self assert: context pc equals: materializedContext pc.
	self assert: context stackPtr equals: materializedContext stackPtr.
	self assert: context method equals: materializedContext method.
	self assert: context receiver equals: materializedContext receiver.
	"I cannot compare by = because MethodContext do not implement it and it will finally use the identity."
	self deny: context sender identicalTo: materializedContext sender.
	self assert: context sender pc identicalTo: materializedContext sender pc.
	self assert: context sender stackPtr equals: materializedContext sender stackPtr
	"etc...."
]

{ #category : #tests }
FLContextSerializationTest >> testContextWithClosure [
	"This test should be improved"
	| context1  materializedContext1  |
	context1 := self class blockClosureContextTesting.
	materializedContext1 := self resultOfSerializeAndMaterialize: context1.
	context1 assertWellMaterializedInto: materializedContext1 in: self.
	

]

{ #category : #tests }
FLContextSerializationTest >> testContextWithClosureAndSender [
	"This test should be improved"
	| context1  materializedContext2  context2 |
	
	context1 := (Context newForMethod: FLPair >> #method1).
	context1 
		initializeWith: 23 
		stackPtr: 1 
		method: FLPair >> #method1 
		receiver:  (FLPair new right: 4; left: 2) 
		sender: nil.
	
	context2 := self class blockClosureContextTestingWithSender: context1.
	materializedContext2 := self resultOfSerializeAndMaterialize: context2.
	context2 assertWellMaterializedInto: materializedContext2 in: self.
	

]

{ #category : #tests }
FLContextSerializationTest >> testContextWithClosureContextWithOutPointerTesting [
	"This test should be improved"

	| context1 materializedContext1 |
	context1 := self class blockClosureContextWithOutPointerTesting.
	self assert: (context1 tempNamed: 'string') equals: 'test'.
	materializedContext1 := self resultOfSerializeAndMaterialize: context1.
	self assert: (materializedContext1 tempNamed: 'string') equals: 'test'.
	context1 assertWellMaterializedInto: materializedContext1 in: self
]

{ #category : #tests }
FLContextSerializationTest >> testContextWithNilPc [
	"This test should be improved"
	| context1  materializedContext1  |
	context1 := (Context newForMethod: FLPair >> #method1).
	context1 
		initializeWith: nil 
		stackPtr: 1 
		method: FLPair >> #method1 
		receiver:  (FLPair new right: 4; left: 2) 
		sender: nil.
		
	materializedContext1 := self resultOfSerializeAndMaterialize: context1.
	context1 assertWellMaterializedInto: materializedContext1 in: self.
]

{ #category : #tests }
FLContextSerializationTest >> testContextWithSender [
	"This test should be improved"
	| context1 context2 materializedContext1 |
	context1 := (Context newForMethod: FLPair >> #method1).
	context1 
		initializeWith: 23 
		stackPtr: 1 
		method: FLPair >> #method1 
		receiver:  (FLPair new right: 4; left: 2) 
		sender: nil.
		
	context2 := (Context newForMethod: FLPair >> #method2).
	context2 
		initializeWith: 18 
		stackPtr: 1 
		method: FLPair >> #method2 
		receiver:  (FLPair new right: 5; left: 6) 
		sender: nil.

	context1 privSender:  context2.
		
	materializedContext1 := self resultOfSerializeAndMaterialize: context1.
	context1 assertWellMaterializedInto: materializedContext1 in: self.
]

{ #category : #tests }
FLContextSerializationTest >> testContextWithTemp [
	"This test should be improved"

	| context1 materializedContext1 |
	context1 := Context newForMethod: FLPair >> #methodWithTemp.
	context1
		initializeWith: nil
		stackPtr: 1
		method: FLPair >> #methodWithTemp
		receiver:
			(FLPair new
				right: 4;
				left: 2)
		sender: nil.
	context1 tempNamed: 'string' put: 'capo'.

	materializedContext1 := self resultOfSerializeAndMaterialize: context1.
	self assert: (materializedContext1 tempNamed: 'string') equals: 'capo'.
	context1 assertWellMaterializedInto: materializedContext1 in: self
]

{ #category : #tests }
FLContextSerializationTest >> testDoIt [
	"Serialization of DoIt methods should be possible by default.
	Note: apparently this test didn't use to terminate some times."

	| context |
	FLPlatform current
		setTestTimeLimitFor: self
		to: 30 seconds.
	[ Smalltalk compiler evaluate: 'self error' ]
		on: Error
		do: [ :error | context := error signalerContext copyStack ].
	self serialize: context
]
