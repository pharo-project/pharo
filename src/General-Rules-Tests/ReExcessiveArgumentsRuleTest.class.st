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
ReExcessiveArgumentsRuleTest >> testRule [

	self assert: (self myCritiquesOnMethod: self class >> #longArgMethod:with:with:with:with:) size equals: 1
]
