Class {
	#name : #TaRenameSlot,
	#superclass : #TaSingleComposition,
	#instVars : [
		'oldName',
		'newName'
	],
	#category : #'TraitsV2-Compositions'
}

{ #category : #'instance creation' }
TaRenameSlot class >> from:oldName to: newName on: aTalent [
	^ self new 
		oldName: oldName;
		newName: newName;
		inner: aTalent;
		yourself.
]

{ #category : #copying }
TaRenameSlot >> copyTraitExpression [
	^ self class from: oldName to: newName on: inner
]

{ #category : #accessing }
TaRenameSlot >> newName [
	^ newName
]

{ #category : #accessing }
TaRenameSlot >> newName: anObject [
	newName := anObject
]

{ #category : #accessing }
TaRenameSlot >> oldName [
	^ oldName
]

{ #category : #accessing }
TaRenameSlot >> oldName: anObject [
	oldName := anObject
]

{ #category : #accessing }
TaRenameSlot >> slots [
	^ inner slots
		collect: [ :e | 
			| slot |
			slot := e asSlot.
			slot name = oldName
				ifTrue: [ slot name: newName ].
			slot ]
]

{ #category : #accessing }
TaRenameSlot >> sourceCodeAt: aSelector [
	| originalSourceCode parseTree |
	originalSourceCode := super sourceCodeAt: aSelector.

	parseTree := RBParser parseMethod: originalSourceCode.

	^ (RBParseTreeRewriter new
		replace: oldName asString with: newName asString;
		executeTree: parseTree;
		tree) formattedCode
]
