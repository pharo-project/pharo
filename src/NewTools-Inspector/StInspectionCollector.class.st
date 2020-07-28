"
I collect inspections from an object which wants to be inspected.
I traverse the hierarchy of the object collecting #extensionPragmas and applying them.

For now, I have two pragmas that receive 2 or 3 arguments: 

- inspectorPresentationOrder:title:
- inspectorPresentationOrder:title:if:

order: inspection appearance order
title: inspection title
if: a block receiving ONE argument that will be evaluated to determine if that inspection needs to be shown.

The arguments of the pragmas are required because the tabs are calculated lazily and this information 
is required before.
"
Class {
	#name : #StInspectionCollector,
	#superclass : #Object,
	#instVars : [
		'inspectedObject'
	],
	#category : #'NewTools-Inspector-Model'
}

{ #category : #defaults }
StInspectionCollector class >> defaultExtensionPragmas [

	^ #(
	inspectorPresentationOrder:title:
	inspectorPresentationOrder:title:if:
	)
]

{ #category : #'instance creation' }
StInspectionCollector class >> on: anObject [

	^ self new 
		inspectedObject: anObject;
		yourself
]

{ #category : #accessing }
StInspectionCollector >> collectPages [ 
	| pragmas |
	
	pragmas := self uniquePragmas: ((self extensionPragmas
		inject: #()
		into: [ :all :each | all, (self collectPragmas: each) ])
		select: [ :eachPragma | self pageShouldBeShown: eachPragma ]). 

	^ (pragmas 
		sort: [ :a :b | (a argumentAt: 1) < (b argumentAt: 1) ])
		collect: [ :eachPragma | 
			SpNotebookPage 
				title: (eachPragma argumentAt: 2)
				provider: [ (self inspectedObject perform: eachPragma methodSelector) asInspection ] ]
]

{ #category : #private }
StInspectionCollector >> collectPragmas: aPragmaName [

	^ Pragma
		allNamed: aPragmaName
		from: self inspectedObject class
		to: ProtoObject
]

{ #category : #private }
StInspectionCollector >> extensionPragmas [

	^ self class defaultExtensionPragmas
]

{ #category : #accessing }
StInspectionCollector >> inspectedObject [
	^ inspectedObject
]

{ #category : #accessing }
StInspectionCollector >> inspectedObject: anObject [
	inspectedObject := anObject
]

{ #category : #private }
StInspectionCollector >> pageShouldBeShown: aPragma [
	| condition |
	
	aPragma numArgs < 3 ifTrue: [ ^ true ].
	
	"3rd argument is #if: and it needs to be a block with one argument"
	condition := aPragma argumentAt: 3.
	^ (condition isSymbol 
		ifTrue: [ condition ]
		ifFalse: [ self class compiler evaluate: condition ])
		value: self inspectedObject
]

{ #category : #private }
StInspectionCollector >> uniquePragmas: aCollection [
	| unique |
	
	unique := OrderedDictionary new.
	aCollection do: [ :each |
		unique
			at: each methodSelector
			ifAbsentPut: [ each ] ].
	^ unique values
]
