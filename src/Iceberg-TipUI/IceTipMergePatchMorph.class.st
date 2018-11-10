"
I redefine part of monticello's PSMCMergePatchMorph to not require to rebuild a tree but use Iceberg's one directly. I also adapt the patch morph to handle files and several packages at the same time.
"
Class {
	#name : #IceTipMergePatchMorph,
	#superclass : #PSMCMergePatchMorph,
	#category : #'Iceberg-TipUI-Merge'
}

{ #category : #accessing }
IceTipMergePatchMorph >> changes [
	"Answer the changes tree roots."
	
	^ model ifNotNil: [ model mergeTree children collect: [ :each | IceMergeListWrapper with: each model: model ] ]
]

{ #category : #testing }
IceTipMergePatchMorph >> hasAnyNonConflictConflicts [
	"Answer whether there are any conflicts not marked as a conflict."

	^ self model mergeTree anySatisfy: [:conflict |
		conflict isConflict and: [ conflict isResolved ] ]
]

{ #category : #testing }
IceTipMergePatchMorph >> hasAnyNonIncomingConflicts [
	"Answer whether there are any conflicts not marked as incoming."

	^ self model mergeTree anySatisfy: [:conflict |
		conflict isConflict and: [ 
			conflict isResolved not or: [conflict isLeftChosen ] ] ]
]

{ #category : #testing }
IceTipMergePatchMorph >> hasAnyNonKeptConflicts [
	"Answer whether there are any conflicts not marked as keep."

	^ self model mergeTree anySatisfy: [:conflict |
		conflict isConflict and: [ 
			conflict isResolved not or: [conflict isRightChosen ] ] ]
]

{ #category : #testing }
IceTipMergePatchMorph >> hasConflicts [
	"Answer whether there are any conflicts."
	
	^ self model mergeTree anySatisfy: [:conflict |
		conflict isConflict ]
]

{ #category : #testing }
IceTipMergePatchMorph >> keepAllCurrentVersion [
	"Mark all conflicts as local."
	
	self model mergeTree do: [:c | 
		c isConflict ifTrue: [ c selectLeft ] ].
	self changed: #changes.
	self updateSource
]

{ #category : #actions }
IceTipMergePatchMorph >> keepCurrentVersion [
	"Mark the conflict as local."
	
	self selectedChangeWrapper chooseLocal.
	self changed: #changes.
	self updateSource
]

{ #category : #testing }
IceTipMergePatchMorph >> markAllAsConflict [
	"Mark all conflicts as unresolved."
	
	self model mergeTree do: [:c | 
		c isConflict ifTrue: [ c clearSelection ] ].
	self changed: #changes.
	self updateSource
]

{ #category : #actions }
IceTipMergePatchMorph >> markAsConflict [
	"Mark the operation as unresolved conflict."
	
	self selectedChangeWrapper item value clearSelection.
	self changed: #changes.
	self updateSource.
]

{ #category : #actions }
IceTipMergePatchMorph >> nextConflict [
	"Answer the next (unresolved) conflict or, nil if none."

	| node firstNode |
	firstNode := self selectedChangeWrapper
		ifNil: [ IceMergeListWrapper with: model mergeTree children first model: model ].
	
	node := firstNode item
		nextNodeSuchThat: [ :item | item value isConflict and: [item value isResolved not] ]
		ifNone: [ ^ nil ].
		
	^ IceMergeListWrapper with: node model: model.
]

{ #category : #testing }
IceTipMergePatchMorph >> notAllConflictsResolved [
	"Answer whether any conflicts are unresolved."
	
	^self model mergeTree anySatisfy: [:item | 
		item isConflict and: [item isResolved not]]
]

{ #category : #actions }
IceTipMergePatchMorph >> selectNextConflict [
	"Select the next conflict after the current selection, if any."

	|next |
	next := self nextConflict ifNil: [^self].
	self changed: {#openPath}, next item path segments
]

{ #category : #accessing }
IceTipMergePatchMorph >> selectedChange [
	"Answer the selected change."

	^(self selectedChangeWrapper ifNil: [^nil]) item value
]

{ #category : #private }
IceTipMergePatchMorph >> selectionHasActualClass [
	"Answer whether the currently selected change has an actual
	class in the image."

	^self selectedChangeWrapper
		ifNil: [false]
		ifNotNil: [:w | w item value contextClass notNil]
]

{ #category : #testing }
IceTipMergePatchMorph >> selectionIsRemoteChosen [
	self selectedChange ifNil: [ ^ false].
	^ self selectedChange isConflict
		and: [ self selectedChange isRightChosen ]
]

{ #category : #updating }
IceTipMergePatchMorph >> updateSource [
	"Update the source difference morph."

	
	|sel|
	sel := self selectedChange.
	self diffMorph allowJoinClicks: false"(sel notNil and: [
		sel isConflict and: [sel  isModification]])".
	
	sel isNil
		ifTrue: [self diffMorph
				from: ''
				to: '']
		ifFalse: [
			self diffMorph
				from: sel rightContents asText
				to: sel leftContents asText
				contextClass: sel contextClass ].

	(sel isNil or: [sel isConflict not]) ifTrue: [^self].
	sel isLeftChosen
		ifTrue: [self diffMorph indicateDst]
		ifFalse: [self diffMorph indicateSrc]
]

{ #category : #testing }
IceTipMergePatchMorph >> useAllIncomingVersion [
	"Mark all conflicts as remote."
	
	self model mergeTree do: [:c | 
		c isConflict ifTrue: [ c selectRight ] ].
	self changed: #changes.
	self updateSource
]
