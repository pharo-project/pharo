Class {
	#name : #ClySubclassLOfClass2FromP1Mock,
	#superclass : #ClyClass2FromP1Mock,
	#classVars : [
		'SubclassClassVar'
	],
	#category : #'Calypso-SystemQueries-Tests-P1WithHierarchy'
}

{ #category : #accessing }
ClySubclassLOfClass2FromP1Mock >> subclassClassVar1Reader [
	^ClassVar1
]

{ #category : #accessing }
ClySubclassLOfClass2FromP1Mock >> subclassClassVar1Writer [
	ClassVar1 := #subclassClassVar1Value
]

{ #category : #accessing }
ClySubclassLOfClass2FromP1Mock >> subclassClassVarReader [
	^SubclassClassVar
]
