"
This Morph is used to show  a collapsed Morph when the World lacks a Taskbar. 


"
Class {
	#name : #CollapsedMorph,
	#superclass : #SystemWindow,
	#instVars : [
		'uncollapsedMorph'
	],
	#category : #'Morphic-Widgets-Windows-Widgets'
}

{ #category : #'collapse/expand' }
CollapsedMorph >> beReplacementFor: aMorph [

	| itsWorld priorPosition |
	(itsWorld := aMorph world) ifNil: [^self].
	uncollapsedMorph := aMorph.
			
	self setLabel: aMorph externalName.
	aMorph delete.
	itsWorld addMorphFront: self.
	self collapseOrExpand.
	(priorPosition := aMorph valueOfProperty: #collapsedPosition ifAbsent: [nil])
	ifNotNil:
		[self position: priorPosition].

]

{ #category : #menu }
CollapsedMorph >> buildWindowMenu [
	"Answer the menu to be put up in response to the user's clicking on the window-menu control in the window title.  Specialized for CollapsedMorphs."

	| aMenu |
	aMenu := UIManager default
					newMenuIn: self for: self.
	aMenu add: 'Change name...' selector: #relabel.
	aMenu addLine.
	aMenu add: 'Send to back' selector: #sendToBack.
	aMenu add: 'Make next-to-topmost' selector: #makeSecondTopmost.
	aMenu addLine.
	self mustNotClose
		ifFalse:
			[aMenu add: 'Make unclosable' selector: #makeUnclosable]
		ifTrue:
			[aMenu add: 'Make closable' translated selector: #makeClosable].
	aMenu
		add: (self isSticky ifTrue: ['Make draggable'] ifFalse: ['Make undraggable']) 
		selector: #toggleStickiness.
	^aMenu
]

{ #category : #'resize/collapse' }
CollapsedMorph >> collapseOrExpand [
	"Toggle the expand/collapsd state of the receiver.  If expanding, copy the window title back to the name of the expanded morph"

	| aWorld |
	isCollapsed
		ifTrue: 
			[uncollapsedMorph setProperty: #collapsedPosition toValue: self position.
			labelString ifNotNil: [uncollapsedMorph setNameTo: labelString].
			mustNotClose := false.	"We're not closing but expanding"
			self delete.
			(aWorld := self currentWorld) addMorphFront: uncollapsedMorph.
			aWorld startSteppingSubmorphsOf: uncollapsedMorph]
		ifFalse:
			[super collapseOrExpand]
]

{ #category : #'collapse/expand' }
CollapsedMorph >> uncollapseToHand [
	"Hand the uncollapsedMorph to the user, placing it in her hand, after remembering appropriate state for possible future use"

	| nakedMorph |
	nakedMorph := uncollapsedMorph.
	uncollapsedMorph := nil.
	nakedMorph setProperty: #collapsedPosition toValue: self position.
	mustNotClose := false.  "so the delete will succeed"
	self delete.
	ActiveHand attachMorph: nakedMorph
]

{ #category : #'resize/collapse' }
CollapsedMorph >> wantsExpandBox [
	"Answer whether I'd like an expand box"

	^ false
]
