"
I am status bar tool for method editors to input method tags for the editing method.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	textMorph:		<Object>
"
Class {
	#name : #ClyMethodTagsEditorMorph,
	#superclass : #ClyStatusBarItemMorph,
	#instVars : [
		'textMorph'
	],
	#category : #'Calypso-SystemTools-Core-Editors-Methods'
}

{ #category : #operations }
ClyMethodTagsEditorMorph >> acceptTags [

	textMorph accept.
	textMorph hasUnacceptedEdits: false
]

{ #category : #building }
ClyMethodTagsEditorMorph >> build [
	self hResizing: #rigid.
	self width: 120.
	textMorph := self theme 
		newTextEntryIn: self 
		for: self 
		get: #printMethodTags 
		set: #setNewMethodTags: 
		class: nil 
		getEnabled: nil 
		help: 'input tags which should be applied to current method'.
	textMorph 
		hResizing: #spaceFill;
		acceptOnFocusChange: false;
		ghostText: 'as yet unclassified'.
	
	self addMorph: textMorph
]

{ #category : #operations }
ClyMethodTagsEditorMorph >> printMethodTags [

	^', ' join: ownerTool methodTags
]

{ #category : #operations }
ClyMethodTagsEditorMorph >> setNewMethodTags: tagsString [

	| tags |
	tags := (',' split: tagsString) collect: #trimBoth as: Array.
	
	ownerTool methodTags: (tags reject: #isEmpty)
]
