Class {
	#name : #TFUFFIDifferentCallingConventionFunctionCallTest,
	#superclass : #TFUFFITestCase,
	#category : #'ThreadedFFI-UFFI-Tests'
}

{ #category : #tests }
TFUFFIDifferentCallingConventionFunctionCallTest >> calloutWithManyArguments: arg1 and: arg2 and: arg3 and: arg4 and: arg5 and: arg6 and: arg7 and: arg8 and: arg9 and: arg10 [
	^ self
		ffiCall:
			#(int w64Convention #(int arg1 , int arg2 , int arg3 , int arg4 , int arg5 , int arg6 , int arg7 , int arg8 , int arg9 , int arg10))
]

{ #category : #private }
TFUFFIDifferentCallingConventionFunctionCallTest >> ffiCallingConvention [

	^ #win64
]

{ #category : #tests }
TFUFFIDifferentCallingConventionFunctionCallTest >> testCallingFunctionWithW64CallingConvention [

	| result |

	"This test is only valid in OSX and X86_64, as calling conventions constants are heavily dependant on the OS & Architecture"
	(OSPlatform current isMacOS and: [ Smalltalk vm architectureName = 'x86_64'])
		ifFalse: [ self skip ].

	result := self
			calloutWithManyArguments: 1
			and: 2
			and: 3
			and: 4
			and: 5
			and: 6
			and: 7
			and: 8
			and: 9
			and: 10.

	self assert: result equals: 55
]
