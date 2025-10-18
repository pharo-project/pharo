"
I implement the toggle edit/view comment in the lower right corner of the class comment pane.
I can be activated by the keyboard shortcut defined in `ClyRichTextClassCommentEditorToolMorph>>buildTextMorph` - command-r

![](https://user-images.githubusercontent.com/17723745/138033718-94ec6db8-f6e3-45fd-93e5-7722261ebc41.png)

"
Class {
	#name : 'ClyEditCommentSwitchMorph',
	#superclass : 'ClyStatusBarItemMorph',
	#instVars : [
		'checkbox'
	],
	#category : 'BeautifulComments-Core',
	#package : 'BeautifulComments',
	#tag : 'Core'
}

{ #category : 'building' }
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

{ #category : 'accessing' }
ClyEditCommentSwitchMorph >> definitionTypeLabel [
	^ 'Toggle Edit / View comment'
]

{ #category : 'building' }
ClyEditCommentSwitchMorph >> status [

	^ ownerTool isRendering
]

{ #category : 'operations' }
ClyEditCommentSwitchMorph >> toggle [
	ownerTool toggleMode.
	ownerTool update
]
