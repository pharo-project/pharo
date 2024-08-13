"
This class contains tests for class `ReExcessiveArgumentsRule`
"
Class {
	#name : 'ReExcessiveArgumentsRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Design Flaws',
	#package : 'General-Rules-Tests',
	#tag : 'Design Flaws'
}

{ #category : 'tests' }
ReExcessiveArgumentsRuleTest >> longArgMethod: arg1 with: arg2 with: arg3 with: arg4 with: arg5 [
	"This method should be critisized due to too many arguments"

	^self
]

{ #category : 'tests' }
ReExcessiveArgumentsRuleTest >> longArgMethodCallingFFI: arg1 with: arg2 with: arg3 with: arg4 with: arg5 [
	"This method should not be critisized as it includes an FFI call"

	^self ffiCall: #(
			int SomeFFI(
     				String* arg1,
         			String* arg2,
     				String* arg3,
     				String* arg4,
        			int arg5)) module: #someModule
]

{ #category : 'tests' }
ReExcessiveArgumentsRuleTest >> testRule [

	| critiques|
	critiques := self myCritiquesOnMethod: self class >> #longArgMethod:with:with:with:with:.
	self assert: critiques size equals: 1
]

{ #category : 'tests' }
ReExcessiveArgumentsRuleTest >> testRuleNotApplyToFFICallingMethods [

	| critiques|
	critiques := self myCritiquesOnMethod: self class >> #longArgMethodCallingFFI:with:with:with:with:.
	self assert: critiques isEmpty
]
