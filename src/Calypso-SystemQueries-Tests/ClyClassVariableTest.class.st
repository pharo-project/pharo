Class {
	#name : #ClyClassVariableTest,
	#superclass : #TestCase,
	#category : #'Calypso-SystemQueries-Tests-Domain'
}

{ #category : #tests }
ClyClassVariableTest >> testDetectingDefiningClassFromClassSide [

	| varBinding var |
	varBinding := ClyClass2FromP1 classVariableNamed: #ClassVar1.
	
	var := ClyClassVariable on: varBinding visibleFrom: ClyClass2FromP1 class.
		
	self assert: var definingClass equals: ClyClass2FromP1
]

{ #category : #tests }
ClyClassVariableTest >> testDetectingDefiningClassFromItself [

	| varBinding var |
	varBinding := ClyClass2FromP1 classVariableNamed: #ClassVar1.
	
	var := ClyClassVariable on: varBinding visibleFrom: ClyClass2FromP1.
		
	self assert: var definingClass equals: ClyClass2FromP1
]

{ #category : #tests }
ClyClassVariableTest >> testDetectingDefiningClassFromSharedPoolUser [

	| varBinding var |
	varBinding := ChronologyConstants classVariableNamed: #DayNames.
	
	var := ClyClassVariable on: varBinding visibleFrom: Date.
		
	self assert: var definingClass equals: ChronologyConstants
]

{ #category : #tests }
ClyClassVariableTest >> testDetectingDefiningClassFromSharedPoolUserClassSide [

	| varBinding var |
	varBinding := ChronologyConstants classVariableNamed: #DayNames.
	
	var := ClyClassVariable on: varBinding visibleFrom: Date class.
		
	self assert: var definingClass equals: ChronologyConstants
]

{ #category : #tests }
ClyClassVariableTest >> testDetectingDefiningClassFromSubclass [

	| varBinding var |
	varBinding := ClyClass2FromP1 classVariableNamed: #ClassVar1.
	
	var := ClyClassVariable on: varBinding visibleFrom: ClySubclassLOfClass2FromP1.
		
	self assert: var definingClass equals: ClyClass2FromP1
]

{ #category : #tests }
ClyClassVariableTest >> testDetectingDefiningClassFromSubclassClassSide [

	| varBinding var |
	varBinding := ClyClass2FromP1 classVariableNamed: #ClassVar1.
	
	var := ClyClassVariable on: varBinding visibleFrom: ClySubclassLOfClass2FromP1 class.
		
	self assert: var definingClass equals: ClyClass2FromP1
]
