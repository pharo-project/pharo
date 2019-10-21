"
A bottom bar to change the editing mode, switch with/without line numbers and switch wrapping mode
"
Class {
	#name : #RubWorkspaceBar,
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
RubWorkspaceBar class >> key [
	^ #workspaceBar
]

{ #category : #accessing }
RubWorkspaceBar >> backgroundColor [
	^ self paragraphProvider backgroundColor darker
]

{ #category : #'managing editing mode' }
RubWorkspaceBar >> chooseEditingMode: anEvent [
	self modeListMenu invokeAt: anEvent position in: self world.
	self updateContents
]

{ #category : #geometry }
RubWorkspaceBar >> computedHeightFrom: aRectangle [
	^ row ifNil: [0] ifNotNil: [row height]
]

{ #category : #geometry }
RubWorkspaceBar >> computedWidthFrom: aRectangle [
	^ aRectangle width
]

{ #category : #'managing editing mode' }
RubWorkspaceBar >> editingModeLabel [
	^ self paragraphProvider editingMode label
]

{ #category : #'managing editing mode' }
RubWorkspaceBar >> editingModeLabelMorph [
	^ StringMorph
		contents: self editingModeLabel
		font: self fontToUse
]

{ #category : #initialization }
RubWorkspaceBar >> initialize [
	super initialize.
	self side: #bottom
]

{ #category : #accessing }
RubWorkspaceBar >> level [
	^ 1
]

{ #category : #geometry }
RubWorkspaceBar >> manageLayoutInBounds: aRectangle [
	| ret |
	ret := super manageLayoutInBounds: aRectangle.
	self updateContents.
	^ ret
]

{ #category : #'managing editing mode' }
RubWorkspaceBar >> modeListMenu [
	"Answer the menu to be presented to select an editing mode"

	| menu |
	menu := MenuMorph new.
	menu add: 'Plain text' target: self selector: #switchToPlainTextMode.
	menu add: 'Smalltalk scripting' target: self selector: #switchToSmalltalkScriptingMode.
	^ menu
]

{ #category : #'submorphs-accessing' }
RubWorkspaceBar >> noteNewOwner: aMorph [
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
RubWorkspaceBar >> spacer: anInteger [
	^ Morph new
		borderWidth: 0;
		color: Color transparent;
		extent: anInteger@3
]

{ #category : #'managing editing mode' }
RubWorkspaceBar >> switchToPlainTextMode [
	self paragraphProvider beForPlainText 
]

{ #category : #'managing editing mode' }
RubWorkspaceBar >> switchToSmalltalkScriptingMode [
	self paragraphProvider beForSmalltalkScripting 
]

{ #category : #accessing }
RubWorkspaceBar >> textColor [
	^ self textArea lineNumbersTextColor muchDarker

]

{ #category : #'event handling' }
RubWorkspaceBar >> updateContents [
	row ifNil: [ ^self ].
	wrappingPolicyMorph contents: self wrappingPolicyLabel.
	editingModeMorph contents: self editingModeLabel.
	lineNumbersDisplayMorph contents: self lineNumbersDisplayLabel.
	row bounds: self innerBounds
]
