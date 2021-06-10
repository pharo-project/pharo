"
A bottom bar to change the editing mode, switch with/without line numbers and switch wrapping mode
"
Class {
	#name : #RubWorkspaceBarRuler,
	#superclass : #RubScrolledTextSideRuler,
	#traits : 'TRubWithWrapPolicy + (TRubWithLineNumber - {#fontToUse})',
	#classTraits : 'TRubWithWrapPolicy classTrait + TRubWithLineNumber classTrait',
	#instVars : [
		'row',
		'wrappingPolicyMorph',
		'editingModeMorph',
		'lineNumbersDisplayMorph'
	],
	#category : #'Tool-Workspace-Utilities'
}

{ #category : #querying }
RubWorkspaceBarRuler class >> key [
	^ #workspaceBar
]

{ #category : #accessing }
RubWorkspaceBarRuler >> backgroundColor [
	^ self paragraphProvider backgroundColor darker
]

{ #category : #'managing editing mode' }
RubWorkspaceBarRuler >> chooseEditingMode: anEvent [
	self modeListMenu invokeAt: anEvent position in: self world.
	self updateContents
]

{ #category : #geometry }
RubWorkspaceBarRuler >> computedHeightFrom: aRectangle [
	^ row ifNil: [0] ifNotNil: [row height]
]

{ #category : #geometry }
RubWorkspaceBarRuler >> computedWidthFrom: aRectangle [
	^ aRectangle width
]

{ #category : #'managing editing mode' }
RubWorkspaceBarRuler >> editingModeLabel [
	^ self paragraphProvider editingMode label
]

{ #category : #'managing editing mode' }
RubWorkspaceBarRuler >> editingModeLabelMorph [
	^ StringMorph
		contents: self editingModeLabel
		font: self fontToUse
]

{ #category : #initialization }
RubWorkspaceBarRuler >> initialize [
	super initialize.
	self side: #bottom
]

{ #category : #accessing }
RubWorkspaceBarRuler >> level [
	^ 1
]

{ #category : #geometry }
RubWorkspaceBarRuler >> manageLayoutInBounds: aRectangle [
	| ret |
	ret := super manageLayoutInBounds: aRectangle.
	self updateContents.
	^ ret
]

{ #category : #'managing editing mode' }
RubWorkspaceBarRuler >> modeListMenu [
	"Answer the menu to be presented to select an editing mode"

	| menu |
	menu := MenuMorph new.
	menu add: 'Plain text' target: self selector: #switchToPlainTextMode.
	menu add: 'Smalltalk scripting' target: self selector: #switchToSmalltalkScriptingMode.
	^ menu
]

{ #category : #'submorphs - accessing' }
RubWorkspaceBarRuler >> noteNewOwner: aMorph [
	super noteNewOwner: aMorph.
	self color: self backgroundColor.
	self paragraphProvider beForSmalltalkScripting.
	wrappingPolicyMorph := self wrappingPolicyLabelMorph.
	editingModeMorph := self editingModeLabelMorph.
	lineNumbersDisplayMorph := self lineNumbersDisplayLabelMorph.
	row := self theme
		newRowIn: self
		for:
			{(self spacer: 2).
			editingModeMorph.
			(self spacer: 2).
			wrappingPolicyMorph.
			(self spacer: 2).
			lineNumbersDisplayMorph.
			(self spacer: 2)}.
	wrappingPolicyMorph on: #mouseDown send: #changeWrappingPolicy to: self.
	editingModeMorph on: #mouseDown send: #chooseEditingMode: to: self.
	lineNumbersDisplayMorph on: #mouseDown send: #changeLineNumbersDisplay to: self.
	row color: Color transparent.
	row hResizing: #spaceFill.
	self addMorph: row
]

{ #category : #private }
RubWorkspaceBarRuler >> spacer: anInteger [
	^ Morph new
		borderWidth: 0;
		color: Color transparent;
		extent: anInteger@3
]

{ #category : #'managing editing mode' }
RubWorkspaceBarRuler >> switchToPlainTextMode [
	self paragraphProvider beForPlainText 
]

{ #category : #'managing editing mode' }
RubWorkspaceBarRuler >> switchToSmalltalkScriptingMode [
	self paragraphProvider beForSmalltalkScripting 
]

{ #category : #accessing }
RubWorkspaceBarRuler >> textColor [
	^ self textArea lineNumbersTextColor muchDarker

]

{ #category : #'event handling' }
RubWorkspaceBarRuler >> updateContents [
	row ifNil: [ ^self ].
	wrappingPolicyMorph contents: self wrappingPolicyLabel.
	editingModeMorph contents: self editingModeLabel.
	lineNumbersDisplayMorph contents: self lineNumbersDisplayLabel.
	row bounds: self innerBounds
]
