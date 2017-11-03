Class {
	#name : #PSMCMergePatchMorph,
	#superclass : #PSMCPatchMorph,
	#category : #Tool-Diff
}

{ #category : #'as yet unclassified' }
PSMCMergePatchMorph >> allConflicts [
	"Answer all conflicts in the model."

	^self model select: [:op | op isConflict]
]

{ #category : #'as yet unclassified' }
PSMCMergePatchMorph >> allConflictsMenu [
	"Answer the all conflicts menu."

	| menu |
	menu := UIManager default newMenuIn: self for: self.
	menu
		addToggle: 'Keep current version of all' translated
		target: self
		selector: #keepAllCurrentVersion
		getStateSelector: nil
		enablementSelector: #hasAnyNonKeptConflicts.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallBackIcon);
		keyText: 'Shift+Cmd+c'.
	menu
		addToggle: 'Use incoming version of all' translated
		target: self
		selector: #useAllIncomingVersion
		getStateSelector: nil
		enablementSelector: #hasAnyNonIncomingConflicts.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallForwardIcon);
		keyText: 'Shift+Cmd+g'.
	menu
		addToggle: 'Mark all as conflict' translated
		target: self
		selector: #markAllAsConflict
		getStateSelector: nil
		enablementSelector: #hasAnyNonConflictConflicts.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallCancelIcon);
		keyText: 'Shift+Cmd+x'.
	^ menu
]

{ #category : #'as yet unclassified' }
PSMCMergePatchMorph >> changeTreeKey: aCharacter [
	"Handle change tree key strokes."

	super changeTreeKey: aCharacter.
	(aCharacter == $a and: [ self selectionIsNotNil]) ifTrue: [
		^ self toggleApplySelectedChange.
		].
	(aCharacter == $c and: [self selectionIsNotNil]) ifTrue: [
		^self keepCurrentVersion].
	(aCharacter == $g and: [self selectionIsNotNil]) ifTrue: [
		^self useIncomingVersion].
	(aCharacter == $x and: [self selectionIsNotNil]) ifTrue: [
		^self markAsConflict].
	(aCharacter == $f and: [self notAllConflictsResolved]) ifTrue: [
		^self selectNextConflict].
	
	(aCharacter == $C and: [self hasAnyNonKeptConflicts]) ifTrue: [
		^self keepAllCurrentVersion].
	(aCharacter == $G and: [self hasAnyNonIncomingConflicts]) ifTrue: [
		^self useAllIncomingVersion].
	(aCharacter == $X and: [self hasAnyNonConflictConflicts]) ifTrue: [
		^self markAllAsConflict]
]

{ #category : #'as yet unclassified' }
PSMCMergePatchMorph >> changesMenu: m [
	"Answer the changes menu."

	| menu |
	menu := super changesMenu: m.
	menu addLine.
	menu
		addToggle: 'Apply this change' translated
		target: self
		selector: #toggleApplySelectedChange
		getStateSelector: #selectionIsRemoteChosen
		enablementSelector: #selectionIsNotNil.
	menu
		addToggle: 'Keep current version' translated
		target: self
		selector: #keepCurrentVersion
		getStateSelector: nil
		enablementSelector: #selectionIsNotNil.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallBackIcon);
		keyText: 'Cmd+c'.
	menu
		addToggle: 'Use incoming version' translated
		target: self
		selector: #useIncomingVersion
		getStateSelector: nil
		enablementSelector: #selectionIsNotNil.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallForwardIcon);
		keyText: 'Cmd+g'.
	menu
		addToggle: 'Mark as conflict' translated
		target: self
		selector: #markAsConflict
		getStateSelector: nil
		enablementSelector: #selectionIsNotNil.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallCancelIcon);
		keyText: 'Cmd+x'.
	menu
		addToggle: 'All conflicts' translated
		target: self
		selector: nil
		getStateSelector: nil
		enablementSelector: #hasConflicts.
	menu lastItem
		font: self theme menuFont;
		subMenu: self allConflictsMenu.
	menu addLine.
	menu
		addToggle: 'Select next conflict' translated
		target: self
		selector: #selectNextConflict
		getStateSelector: nil
		enablementSelector: #notAllConflictsResolved.
	menu lastItem
		font: self theme menuFont;
		icon: (self iconNamed: #smallRightFlushIcon);
		keyText: 'Cmd+f'.
	^ menu
]

{ #category : #hooks }
PSMCMergePatchMorph >> diffMorphClass [
	"Answer a the class to use for a new diff morph."

	^MergeDiffMorph
]

{ #category : #'as yet unclassified' }
PSMCMergePatchMorph >> hasAnyNonConflictConflicts [
	"Answer whether there are any conflicts not marked as a conflict."

	^self allConflicts anySatisfy: [:conflict |
		conflict isResolved]
]

{ #category : #'as yet unclassified' }
PSMCMergePatchMorph >> hasAnyNonIncomingConflicts [
	"Answer whether there are any conflicts not marked as incoming."

	^self allConflicts anySatisfy: [:conflict |
		conflict isResolved not or: [conflict localChosen]]
]

{ #category : #'as yet unclassified' }
PSMCMergePatchMorph >> hasAnyNonKeptConflicts [
	"Answer whether there are any conflicts not marked as keep."

	^self allConflicts anySatisfy: [:conflict |
		conflict isResolved not or: [conflict remoteChosen]]
]

{ #category : #'as yet unclassified' }
PSMCMergePatchMorph >> hasConflicts [
	"Answer whether there are any conflicts."

	^self allConflicts notEmpty
]

{ #category : #'as yet unclassified' }
PSMCMergePatchMorph >> keepAllCurrentVersion [
	"Mark all conflicts as local."
	
	self allConflicts do: [:c | c chooseLocal].
	self changed: #changes.
	self updateSource
]

{ #category : #'as yet unclassified' }
PSMCMergePatchMorph >> keepCurrentVersion [
	"Mark the conflict as local."
	
	self selectedChangeWrapper chooseLocal.
	self changed: #changes.
	self updateSource
]

{ #category : #'as yet unclassified' }
PSMCMergePatchMorph >> markAllAsConflict [
	"Mark all conflicts as unresolved."
	
	self allConflicts do: [:c | c clearChoice].
	self changed: #changes.
	self updateSource
]

{ #category : #'as yet unclassified' }
PSMCMergePatchMorph >> markAsConflict [
	"Mark the operation as unresolved conflict."
	
	self selectedChangeWrapper operation beConflict.
	self changed: #changes.
	self updateSource.
]

{ #category : #'as yet unclassified' }
PSMCMergePatchMorph >> nextConflict [
	"Answer the next (unresolved) conflict or, nil if none."

	|coll current index|
	current := self selectedChangeWrapper.
	index := current isNil
		ifTrue: [0]
		ifFalse: [self model indexOf: current item].
	coll := (self model
			copyFrom: index + 1
			to: self model size),
		(self model copyFrom: 1 to: index).
	^coll detect: [:item | item isConflict and: [item isResolved not]] ifNone: [nil]
]

{ #category : #'as yet unclassified' }
PSMCMergePatchMorph >> notAllConflictsResolved [
	"Answer whether any conflicts are unresolved."
	
	^self model anySatisfy: [:item | item isConflict and: [item isResolved not]]
]

{ #category : #'as yet unclassified' }
PSMCMergePatchMorph >> selectNextConflict [
	"Select the next conflict after the current selection, if any."

	|next op def path|
	next := self nextConflict ifNil: [^self].
	op := next operation.
	def := next remoteDefinition ifNil: [next localDefinition].
	path := {#changes. #openPath}.
	def isMethodDefinition ifTrue: [path := path, {def fullClassName}].
	path := path, {op shortSummary}.
	self changed: path
]

{ #category : #'as yet unclassified' }
PSMCMergePatchMorph >> selectionIsNotNil [

	^ self selectedChange notNil
]

{ #category : #'as yet unclassified' }
PSMCMergePatchMorph >> selectionIsRemoteChosen [
	self selectedChange ifNil: [ ^ false].
	^ self selectedChange remoteChosen
]

{ #category : #'as yet unclassified' }
PSMCMergePatchMorph >> toggleApplySelectedChange [

	self selectedChange remoteChosen 
		ifTrue: [ self selectedChange chooseLocal ]
		ifFalse: [ self selectedChange chooseRemote ].
	self changed: #changes.
	self updateSource
]

{ #category : #'as yet unclassified' }
PSMCMergePatchMorph >> updateSource [
	"Update the source difference morph."

	|sel|
	sel := self selectedChangeWrapper.
	self diffMorph allowJoinClicks: (sel notNil and: [
		sel isConflict and: [sel operation isModification]]).
	super updateSource.
	(sel isNil or: [sel isConflict not]) ifTrue: [^self].
	sel localChosen
		ifTrue: [self diffMorph indicateSrc]
		ifFalse: [self diffMorph indicateDst]
]

{ #category : #'as yet unclassified' }
PSMCMergePatchMorph >> useAllIncomingVersion [
	"Mark all conflicts as remote."
	
	self allConflicts do: [:c | c chooseRemote].
	self changed: #changes.
	self updateSource
]

{ #category : #'as yet unclassified' }
PSMCMergePatchMorph >> useIncomingVersion [
	"Mark the conflict as remote."
	
	self selectedChangeWrapper chooseRemote.
	self changed: #changes.
	self updateSource
]
