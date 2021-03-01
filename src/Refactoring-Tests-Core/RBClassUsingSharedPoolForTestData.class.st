Class {
	#name : #RBClassUsingSharedPoolForTestData,
	#superclass : #Object,
	#traits : 'RBDummy',
	#classTraits : 'RBDummy classTrait',
	#pools : [
		'RBSharedPoolForTestData'
	],
	#category : #'Refactoring-Tests-Core-Data'
}

{ #category : #'as yet unclassified' }
RBClassUsingSharedPoolForTestData >> msg3 [

	^ Var1
]
