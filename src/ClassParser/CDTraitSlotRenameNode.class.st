Class {
	#name : #CDTraitSlotRenameNode,
	#superclass : #CDTraitCompositionNode,
	#instVars : [
		'renames'
	],
	#category : #'ClassParser-Model'
}

{ #category : #accessing }
CDTraitSlotRenameNode >> renames [

	^ renames
]

{ #category : #accessing }
CDTraitSlotRenameNode >> renames: anObject [

	renames := anObject
]
