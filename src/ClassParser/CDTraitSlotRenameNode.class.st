"
MyTrait @@ {#ivar -> ivar2 }
"
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

{ #category : #printing }
CDTraitSlotRenameNode >> toString [
	^ subject name ,' @@ ', renames asString
]
