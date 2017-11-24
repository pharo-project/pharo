"
A pool used for testing purposes
"
Class {
	#name : #PoolDefiner2,
	#superclass : #SharedPool,
	#classVars : [
		'Author',
		'VariableInPoolDefiner2'
	],
	#category : #'Kernel-Tests-Classes'
}

{ #category : #initialization }
PoolDefiner2 class >> initialize [
	"self initialize"
	
	VariableInPoolDefiner2 := 33.
	Author := 'NotDucasse'.
]
