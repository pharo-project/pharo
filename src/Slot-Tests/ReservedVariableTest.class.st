Class {
	#name : #ReservedVariableTest,
	#superclass : #TestCase,
	#category : #'Slot-Tests-VariablesAndSlots'
}

{ #category : #tests }
ReservedVariableTest >> testNameIsReserved [
 	"we have these reserved vars: self, super, thisContext"
 	self assert: (ReservedVariable nameIsReserved: 'self').
 	self assert: (ReservedVariable nameIsReserved: 'super').
 	self assert: (ReservedVariable nameIsReserved: 'thisContext').

  	"true and false and nil are compiled as literals, not reserved vars"
 	self deny: (ReservedVariable nameIsReserved: 'true').
 	self deny: (ReservedVariable nameIsReserved: 'false').
 	self deny: (ReservedVariable nameIsReserved: 'nil')
]
