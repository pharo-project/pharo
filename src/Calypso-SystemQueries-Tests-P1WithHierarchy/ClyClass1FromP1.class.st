Class {
	#name : #ClyClass1FromP1,
	#superclass : #Object,
	#instVars : [
		'instanceSideVar2',
		'instanceSideVar1'
	],
	#category : #'Calypso-SystemQueries-Tests-P1WithHierarchy'
}

{ #category : #'as yet unclassified' }
ClyClass1FromP1 class >> classSideMethodFromClass1 [
]

{ #category : #'var accessors' }
ClyClass1FromP1 >> instanceSideVar1ReaderMethod [
	^instanceSideVar1 
]

{ #category : #'var accessors' }
ClyClass1FromP1 >> instanceSideVar1WriterMethod [
	instanceSideVar1 := #var1Value
]

{ #category : #superclassTag1 }
ClyClass1FromP1 >> superclassTag1Method [
]

{ #category : #tag1 }
ClyClass1FromP1 >> tag1Method1 [
]
