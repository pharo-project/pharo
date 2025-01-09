Class {
	#name : 'OCIRBuilderTest',
	#superclass : 'AbstractEnvironmentTestCase',
	#classVars : [
		'TestToPush'
	],
	#category : 'OpalCompiler-Tests-IR',
	#package : 'OpalCompiler-Tests',
	#tag : 'IR'
}

{ #category : 'accessing' }
OCIRBuilderTest class >> compilerClass [
	^ OpalCompiler
]

{ #category : 'accessing' }
OCIRBuilderTest class >> testToPush [
	^TestToPush
]

{ #category : 'accessing' }
OCIRBuilderTest class >> testToPush: anObject [

	TestToPush := anObject
]

{ #category : 'tests - builder' }
OCIRBuilderTest >> testBuildMethod [
	|  aCompiledMethod |

	aCompiledMethod := OCIRBuilder buildMethod: [ : builder |
		builder pushLiteral: 3;
		pushDup;
		send: #=;
		returnTop].

	self assert: (aCompiledMethod isKindOf: CompiledMethod)
]

{ #category : 'tests' }
OCIRBuilderTest >> testDup [

	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder buildIR: [ : builder |
		builder pushLiteral: 3;
		pushDup;
		send: #=;
		returnTop].

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
  	self assert: (aCompiledMethod valueWithReceiver: nil ).
	^iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testInstVar [
	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushInstVar: 1;
		pushInstVar: 2;
		send: #+;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: (aCompiledMethod valueWithReceiver: 3 @ 4 ) equals: 7.
	^ iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testJumpAheadTo [
	| iRMethod aCompiledMethod iRBuilder |
	iRBuilder := OCIRBuilder new.
	iRMethod := iRBuilder
		pushLiteral: 2;
		pushLiteral: 1;
		send: #+;
		jumpAheadTo: #end;
		pushLiteral: 3;
		jumpAheadTarget: #end;
		returnTop;
		ir.


	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: (aCompiledMethod valueWithReceiver: nil) equals: 3.
	^ iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testJumpAheadToIf [

	| iRMethod aCompiledMethod |

	iRMethod := OCIRBuilder new
		pushReceiver;
		pushLiteral: true;
		jumpAheadTo: #end if: true;
		returnTop;
		jumpAheadTarget: #end;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
 	self assert: ((aCompiledMethod valueWithReceiver: nil) isNil).
	^iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testJumpBackTo [

	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushReceiver;
		pushLiteral: false;
		jumpBackTarget: #begin;
		jumpAheadTo: #end if: true;
		pushLiteral: true;
		jumpBackTo: #begin;
		jumpAheadTarget: #end;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: ((aCompiledMethod valueWithReceiver: nil) isNil).
	^iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testLiteralArray [
	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushLiteral: #(test 4 you);
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: (aCompiledMethod valueWithReceiver: nil) equals: #(test 4 you).
	^ iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testLiteralBoolean [

	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushLiteral: true;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: (aCompiledMethod valueWithReceiver: nil).
	^iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testLiteralCharacter [
	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushLiteral: $e;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: (aCompiledMethod valueWithReceiver: nil) equals: $e.
	^ iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testLiteralFloat [

	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushLiteral: 2.0;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
 	self assert: ((aCompiledMethod valueWithReceiver: nil) closeTo: 2.0).
	^iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testLiteralInteger [
	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushLiteral: 2;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: (aCompiledMethod valueWithReceiver: nil) equals: 2.
	^ iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testLiteralNil [

	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushLiteral: nil;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.

	self assert: (aCompiledMethod isKindOf: CompiledMethod).
     self assert: ((aCompiledMethod valueWithReceiver: 4) isNil).
	^iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testLiteralString [
	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushLiteral: 'hello';
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.

	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: (aCompiledMethod valueWithReceiver: nil) equals: 'hello'.
	^ iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testLiteralSymbol [
	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushLiteral: #you;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.

	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: (aCompiledMethod valueWithReceiver: nil) equals: #you.
	^ iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testLiteralVariableClass [
	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushLiteralVariable: Object binding;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.

	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: (aCompiledMethod valueWithReceiver: nil) equals: Object.
	^ iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testLiteralVariableClassVariable [
	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushLiteralVariable: (OCOpalExamples classVariableNamed: #ExampleClassVariable);
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.

	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	OCOpalExamples new classVariable: #tag.
	self assert: (aCompiledMethod valueWithReceiver: nil) equals: #tag.
	OCOpalExamples reset.
	^ iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testLiteralVariableGlobale [
	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushLiteralVariable: (testingEnvironment associationAt: #Smalltalk);
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.

	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: (aCompiledMethod valueWithReceiver: nil) equals: Smalltalk.
	^ iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testPopTop [

	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushReceiver;
		pushLiteral: false;
		popTop;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: ((aCompiledMethod valueWithReceiver: nil) isNil).
	^iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testPushConsArray [
	| iRMethod aCompiledMethod receiver |
	iRMethod := OCIRBuilder new
		pushReceiver;
		pushConsArray: 1;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.

	receiver := 5 @ 8.

	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: (aCompiledMethod valueWithReceiver: receiver) first identicalTo: receiver.
	^ iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testPushConsArray2 [
	| iRMethod aCompiledMethod receiver |
	iRMethod := OCIRBuilder new
		pushLiteral: 'hi!';
		pushConsArray: 1;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.

	receiver := 5 @ 8.

	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: (aCompiledMethod valueWithReceiver: receiver) equals: #('hi!').
	^ iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testPushNewArray [

	| iRMethod aCompiledMethod receiver |
	iRMethod := OCIRBuilder new

		pushNewArray: 1;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.

	receiver :=  (5@8).

	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: ((aCompiledMethod valueWithReceiver: receiver) first isNil).
	^iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testPushSelf [
	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushReceiver;
		send: #class;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.


	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: (aCompiledMethod valueWithReceiver: nil) identicalTo: UndefinedObject.
	^ iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testPushTempArgument [
	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		numArgs: 2;
		addTemps: #(a b);
		pushTemp: #a;
		pushTemp: #b;
		send: #+;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.


	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: (aCompiledMethod valueWithReceiver: nil arguments: #(2 8)) equals: 10.
	^ iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testPushTempTemp [

	| iRMethod aCompiledMethod  |
	iRMethod := OCIRBuilder new
		addTemps: #(a);
		pushTemp: #a;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.


	self assert: (aCompiledMethod isKindOf: CompiledMethod).
     self assert: ((aCompiledMethod valueWithReceiver: 5) isNil).
	^iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testPushThisContext [
	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushThisContext;
		send: #receiver;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: (aCompiledMethod valueWithReceiver: 5) equals: 5.
	^ iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testPushThisProcess [

	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushThisProcess;
		send: #class;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: (aCompiledMethod valueWithReceiver: nil) equals: Process.
	^ iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testReturnInstVar [
	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushInstVar: 1;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: (aCompiledMethod valueWithReceiver: 3 @ 4) equals: 3.
	^ iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testReturnTop [

	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushLiteral: false;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self deny: (aCompiledMethod valueWithReceiver: nil).
	^iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testSendSuper [

	| iRMethod aCompiledMethod  |
	iRMethod := OCIRBuilder new
		pushReceiver;
		send: #isThisEverCalled toSuperOf: self class;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.


	self assert: (aCompiledMethod isKindOf: CompiledMethod).
     self should: [(aCompiledMethod valueWithReceiver: (self class new))] raise: Error.
	^iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testStoreIntoVariable [
	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushLiteral: 4;
		storeIntoLiteralVariable: (self class bindingOf: #TestToPush);
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.


	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	aCompiledMethod valueWithReceiver: nil.
	self assert: self class testToPush equals: 4.
	self class testToPush: nil.
	^ iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testStoreIvar [
	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushLiteral: 34;
		storeInstVar: 2;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: (aCompiledMethod valueWithReceiver: self) equals: 34.
	^ iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testStorePopIntoVariable [
	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushLiteral: 4;
		storeIntoLiteralVariable: (self class bindingOf: #TestToPush);
		popTop;
		pushLiteralVariable: (self class bindingOf: #TestToPush);
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.


	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	aCompiledMethod valueWithReceiver: nil.
	self assert: self class testToPush equals: 4.
	self class testToPush: nil.
	^ iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testStorePopIvar [
	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		pushLiteral: 34;
		storeInstVar: 2;
		popTop;
		pushInstVar: 2;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: (aCompiledMethod valueWithReceiver: self) equals: 34.
	^ iRMethod
]

{ #category : 'tests' }
OCIRBuilderTest >> testStoreTemp [
	| iRMethod aCompiledMethod |
	iRMethod := OCIRBuilder new
		addTemps: #(a);
		pushLiteral: 34;
		storeTemp: #a;
		popTop;
		pushTemp: #a;
		returnTop;
		ir.

	aCompiledMethod := iRMethod compiledMethod.
	self assert: (aCompiledMethod isKindOf: CompiledMethod).
	self assert: (aCompiledMethod valueWithReceiver: nil) equals: 34.
	^ iRMethod
]
