Class {
	#name : #ClySubclassLOfClass2FromP1,
	#superclass : #ClyClass2FromP1,
	#classVars : [
		'SubclassClassVar'
	],
	#category : #'Calypso-SystemQueries-Tests-P1WithHierarchy'
}

{ #category : #'as yet unclassified' }
ClySubclassLOfClass2FromP1 >> subclassClassVar1Reader [
	^ClassVar1
]

{ #category : #'as yet unclassified' }
ClySubclassLOfClass2FromP1 >> subclassClassVar1Writer [
	ClassVar1 := #subclassClassVar1Value
]
