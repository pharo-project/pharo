"
I'm a simple morph to be added in in a comment editor. I let the user toggle edit / view view of the comment pane.
"
Class {
	#name : #ClyEditCommentSwitchMorph,
	#superclass : #ClyStatusBarItemMorph,
	#instVars : [
		'checkbox'
	],
	#category : #'Microdown-Calypso'
}

{ #category : #building }
ClyEditCommentSwitchMorph >> build [
	
	checkbox := CheckboxMorph 
		on: self selected: #status changeSelected: #toggle.
	checkbox
		getLabelSelector: #definitionTypeLabel;
		labelClickable: true;
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap;
		setBalloonText:	 'Click me to edit comment'.
	self addMorph: checkbox
]

{ #category : #accessing }
ClyEditCommentSwitchMorph >> definitionTypeLabel [
	^ 'Toggle Edit / View comment'
]

{ #category : #building }
ClyEditCommentSwitchMorph >> status [

	^ ownerTool isRendering
]

{ #category : #operations }
ClyEditCommentSwitchMorph >> toggle [
	ownerTool toggleMode.
	ownerTool update
]
