"
A ReservedVariableTest is a test class for testing the behavior of ReservedVariable
"
Class {
	#name : #ReservedVariableTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Semantic'
}

{ #category : #tests }
ReservedVariableTest >> testNameIsReserved [
	"we have these reserved vars: self, super, thisContext"
	self assert: (ReservedVariable nameIsReserved: 'self').
	self assert: (ReservedVariable nameIsReserved: 'super').
	self assert: (ReservedVariable nameIsReserved: 'thisContext').
	
	"true and false and nil are compiled as lierals, not reserved vars"
	self deny: (ReservedVariable nameIsReserved: 'true').
	self deny: (ReservedVariable nameIsReserved: 'false').
	self deny: (ReservedVariable nameIsReserved: 'nil').
]
