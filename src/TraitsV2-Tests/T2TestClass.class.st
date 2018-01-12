Class {
	#name : #T2TestClass,
	#superclass : #Object,
	#instVars : [
		'somethingElse'
	],
	#category : #'TraitsV2-Tests-Classes'
}

{ #category : #'as yet unclassified' }
T2TestClass class >> someClassMessage [
	^ 'aClassMessage'.
]

{ #category : #'as yet unclassified' }
T2TestClass >> hello [
	^ 'hello'
]
