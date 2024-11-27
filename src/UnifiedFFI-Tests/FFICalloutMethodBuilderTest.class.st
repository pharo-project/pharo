"
Tests for FFICalloutMethodBuilder
"
Class {
	#name : 'FFICalloutMethodBuilderTest',
	#superclass : 'TestCase',
	#instVars : [
		'architecture'
	],
	#category : 'UnifiedFFI-Tests-Tests',
	#package : 'UnifiedFFI-Tests',
	#tag : 'Tests'
}

{ #category : 'factory' }
FFICalloutMethodBuilderTest >> builder [
	^ (TFCalloutAPI inUFFIContext: nil) newBuilder
		requestor: FFICalloutMethodBuilderTestRequestor new;
		yourself
]

{ #category : 'running' }
FFICalloutMethodBuilderTest >> setUp [
	super setUp.
	architecture := FFIArchitecture forCurrentArchitecture
]

{ #category : 'tests' }
FFICalloutMethodBuilderTest >> testAllAtomicTypesCall [
	| result tfFunction |

	result := self builder build: [ :builder |
		builder
			signature: #( void method1 (bool var1, byte var2, char var3, double var4, float var5, long var6, longlong var7, sbyte var8, schar var9, short var10, ulong var11, ulonglong var12, ushort var13) );
			sender: (FFICalloutMethodBuilderTestContext new
				selector: #methodV1:v2:v3:v4:v5:v6:v7:v8:v9:v10:v11:v12:v13:;
				argumentNames: #('var1' 'var2' 'var3' 'var4' 'var5' 'var6' 'var7' 'var8' 'var9' 'var10' 'var11' 'var12' 'var13');
				methodClass: Object;
				yourself )].

	self assert: result isNotNil.
	self assert: result isCompiledMethod.
	self assert: result primitive equals: 0.

	tfFunction := result literals second.

	self assert: tfFunction functionName equals: 'method1'.
	self assert: tfFunction definition parameterTypes asArray equals: {
		TFDerivedType boolean.
		TFBasicType uint8.
		TFBasicType schar.
		TFBasicType double.
		TFBasicType float.
		TFBasicType slong.
		TFBasicType sint64.
		TFBasicType sint8.
		TFBasicType sint8.
		TFBasicType sint16.
		TFBasicType ulong.
		TFBasicType uint64.
		TFBasicType uint16}
]

{ #category : 'tests' }
FFICalloutMethodBuilderTest >> testCallCreateObject [
	| result |

	result := self builder build: [ :builder |
		builder requestor requestor: FFICalloutObjectForTest.
		builder
			signature: #( #FFICalloutObjectForTest method1( int var1 ) );
			sender: (FFICalloutMethodBuilderTestContext new
				selector: #method1:;
				argumentNames: #('var1');
				methodClass: FFICalloutObjectForTest;
				yourself )].

	self assert: result isNotNil.
	self assert: result isCompiledMethod.
	self assert: result primitive equals: 0.
	self assert: result literals second functionName equals: 'method1'.
	self assert: result literals second definition returnType equals: TFBasicType pointer.
	self assert: result literals second definition parameterTypes first equals: TFBasicType sint32
]

{ #category : 'tests' }
FFICalloutMethodBuilderTest >> testCallReturningEnumeration [
	| result tfFunction |

	result := self builder build: [ :builder |
		builder requestor requestor: FFICalloutObjectForTest.
		builder
			signature: #( #FFITestEnumeration method1( int var1 ) );
			sender: (FFICalloutMethodBuilderTestContext new
				selector: #method1:;
				argumentNames: #('var1');
				methodClass: FFICalloutObjectForTest;
				yourself )].

	self assert: result isNotNil.
	self assert: result isCompiledMethod.
	self assert: result primitive equals: 0.

	tfFunction := result literals second.

	self assert: tfFunction functionName equals: 'method1'.
	self assert: tfFunction definition returnType equals: TFBasicType uint32
]

{ #category : 'tests' }
FFICalloutMethodBuilderTest >> testCallSimple [
	| result tfFunction |

	result := self builder build: [ :builder |
		builder
			signature: #( void method1 ( int var1 ) );
			sender: (FFICalloutMethodBuilderTestContext new
				selector: #method1:;
				argumentNames: #('var1');
				methodClass: Object;
				yourself )].

	self assert: result isNotNil.
	self assert: result isCompiledMethod.
	self assert: result primitive equals: 0.
	tfFunction := result literals second.

	self assert: tfFunction functionName equals: 'method1'.
	self assert: tfFunction definition returnType equals: TFBasicType void.
	self assert: tfFunction definition parameterTypes asArray equals: { TFBasicType sint32 }
]

{ #category : 'tests' }
FFICalloutMethodBuilderTest >> testCallWithConstant [
	| result tfFunction |

	result := self builder build: [ :builder |
		builder
			signature: #( void method1 ( int var1, 42 ) );
			sender: (FFICalloutMethodBuilderTestContext new
				selector: #method1:value:;
				argumentNames: #('var1' 'var2');
				methodClass: Object;
				yourself )].

	self assert: result isNotNil.
	self assert: result isCompiledMethod.
	self assert: result primitive equals: 0.

	tfFunction := result literals second.

	self assert: tfFunction functionName equals: 'method1'.
	self assert: tfFunction definition returnType equals: TFBasicType void.
	self assert: tfFunction definition parameterTypes asArray equals: { TFBasicType sint32. TFBasicType uint32 }
]

{ #category : 'tests' }
FFICalloutMethodBuilderTest >> testCallWithIntegerConstantAsPointerIsOptimized [

	| result pushInstruction |
	result := self builder build: [ :builder |
		builder requestor requestor: FFICalloutObjectForTest.
		builder
			signature: #( void method1 ( void* 0 ) );
			sender: (FFICalloutMethodBuilderTestContext new
				selector: #method1:value:;
				argumentNames: #();
				methodClass: FFICalloutObjectForTest;
				yourself )].

	pushInstruction := result decompileIR startSequence at: 3.
	
	self assert: pushInstruction isPushLiteral.
	self assert: pushInstruction literal equals: 0 tfPointerAddress 
	
]

{ #category : 'tests' }
FFICalloutMethodBuilderTest >> testCallWithObject [
	| result tfFunction |

	result := self builder build: [ :builder |
		builder requestor requestor: FFICalloutObjectForTest.
		builder
			signature: #( void method1 ( #FFICalloutObjectForTest var1, int *var2 ) );
			sender: (FFICalloutMethodBuilderTestContext new
				selector: #method1:value:;
				argumentNames: #('var1' 'var2');
				methodClass: FFICalloutObjectForTest;
				yourself )].

	self assert: result isNotNil.
	self assert: result isCompiledMethod.
	self assert: result primitive equals: 0.
	tfFunction := result literals second.

	self assert: tfFunction functionName equals: 'method1'.
	self assert: tfFunction definition returnType equals: TFBasicType void.
	self assert: tfFunction definition parameterTypes last equals: TFBasicType pointer
]

{ #category : 'tests' }
FFICalloutMethodBuilderTest >> testCallWithPointer [
	| result tfFunction |

	result := self builder build: [ :builder |
		builder
			signature: #( void method1 ( int var1, int *var2 ) );
			sender: (FFICalloutMethodBuilderTestContext new
				selector: #method1:value:;
				argumentNames: #('var1' 'var2');
				methodClass: Object;
				yourself )].

	self assert: result isNotNil.
	self assert: result isCompiledMethod.
	self assert: result primitive equals: 0.

	tfFunction := result literals second.

	self assert: tfFunction functionName equals: 'method1'.
	self assert: tfFunction definition returnType equals: TFBasicType void.
	self assert: tfFunction definition parameterTypes last equals: TFBasicType pointer
]

{ #category : 'tests' }
FFICalloutMethodBuilderTest >> testCallWithPointerPointer [
	| result tfFunction |

	result := self builder build: [ :builder |
		builder
			signature: #( void method1 ( long **var1 ) );
			sender: (FFICalloutMethodBuilderTestContext new
				selector: #method1:;
				argumentNames: #('var1');
				methodClass: Object;
				yourself )].

	self assert: result isNotNil.
	self assert: result isCompiledMethod.
	self assert: result primitive equals: 0.

	tfFunction := result literals second.

	self assert: tfFunction functionName equals: 'method1'.
	self assert: tfFunction definition returnType equals: TFBasicType void.
	self assert: tfFunction definition parameterTypes last equals: TFBasicType pointer
]

{ #category : 'tests' }
FFICalloutMethodBuilderTest >> testCallWithSelf [
	| result tfFunction |

	result := self builder build: [ :builder |
		builder requestor requestor: FFICalloutObjectForTest.
		builder
			signature: #( void method1 ( self, int *var2 ) );
			sender: (FFICalloutMethodBuilderTestContext new
				selector: #method1:value:;
				argumentNames: #('var1' 'var2');
				methodClass: FFICalloutObjectForTest;
				yourself )].

	self assert: result isNotNil.
	self assert: result isCompiledMethod.
	self assert: result primitive equals: 0.

	tfFunction := result literals second.

	self assert: tfFunction functionName equals: 'method1'.
	self assert: tfFunction definition returnType equals: TFBasicType void.
	self assert: tfFunction definition parameterTypes asArray equals: { TFBasicType pointer. TFBasicType pointer }
]
