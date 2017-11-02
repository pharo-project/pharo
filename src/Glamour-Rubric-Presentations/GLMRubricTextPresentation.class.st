"
A presentation that displays text using Rubric.


"
Class {
	#name : #GLMRubricTextPresentation,
	#superclass : #GLMTextualPresentation,
	#instVars : [
		'withLineNumbers',
		'withAnnotation',
		'wrapped',
		'tabWidth',
		'withColumns',
		'textSegments',
		'primarySelectionInterval',
		'withGhostText'
	],
	#category : 'Glamour-Rubric-Presentations'
}

{ #category : #accessing }
GLMRubricTextPresentation >> addTextSegment: aTextSegmentMorph [
	self textSegments add: aTextSegmentMorph
]

{ #category : #'actions text' }
GLMRubricTextPresentation >> clearUserEdits [
	self flag: 'Should remove the need for using #defer:. Currently it used as
	the rendered suspends all announcements for a presentation when it handles a text changed request.'.
	UIManager default defer: [ 
		self announce: (GLMTextEditorRequest new 
			action: [ :anEditor | anEditor model clearUserEdits]) ]
]

{ #category : #accessing }
GLMRubricTextPresentation >> codeEditingSelectionActions [
	
	^ { 
		(GLMGenericAction new) 
				title: 'Cut'; 
				action: [ :aPresentation |
					aPresentation cut ]; 
				iconName: #smallCutIcon;
				shortcut: $x; 
				withSeparatorBefore;
				yourself.
		(GLMGenericAction new) 
				title: 'Copy';
				action: [ :aPresentation |
					aPresentation copySelection ]; 
				iconName: #smallCopyIcon;
				shortcut: $c; 
				yourself.
		(GLMGenericAction new) 
				title: 'Paste';
				action: [ :aPresentation |
					aPresentation paste ]; 
				iconName: #smallPasteIcon;
				shortcut: $v; 
				yourself.
		(GLMGenericAction new) 
				title: 'Paste...';
				action: [ :aPresentation |
					aPresentation pasteRecent  ]; 
				iconName: #smallCopyIcon;
				withSeparatorAfter;
				yourself. }
]

{ #category : #'actions text' }
GLMRubricTextPresentation >> copySelection [

	self announce: (GLMTextEditorRequest for: #copySelection)
]

{ #category : #'actions text' }
GLMRubricTextPresentation >> cut [

	self announce: (GLMTextEditorRequest for: #cut)
]

{ #category : #accessing }
GLMRubricTextPresentation >> defaultSelectionActions [

	^ self executionSelectionActions
	
]

{ #category : #'actions text' }
GLMRubricTextPresentation >> flash [

	self announce: (GLMTextEditorRequest new 
		action: [ :anEditor | anEditor flash])
]

{ #category : #'actions text' }
GLMRubricTextPresentation >> hasUnacceptedEdits [

	self announce: (GLMTextEditorRequest new 
		action: [ :anEditor | ^ anEditor model hasUnacceptedEdits])
]

{ #category : #'actions text' }
GLMRubricTextPresentation >> paste [

	self announce: (GLMTextEditorRequest for: #paste)
]

{ #category : #'actions text' }
GLMRubricTextPresentation >> pasteRecent [

	self announce: (GLMTextEditorRequest for: #pasteRecent)
]

{ #category : #accessing }
GLMRubricTextPresentation >> primarySelectionInterval [
	^ primarySelectionInterval
]

{ #category : #accessing }
GLMRubricTextPresentation >> primarySelectionInterval: anInterval [
	primarySelectionInterval := anInterval
]

{ #category : #rendering }
GLMRubricTextPresentation >> renderGlamorouslyOn: aRenderer [
	self registerAnnouncements.
	^ aRenderer renderRubricTextPresentation: self
]

{ #category : #accessing }
GLMRubricTextPresentation >> scrollAtIndex: anIndex [
	self primarySelectionInterval: (anIndex to: anIndex - 1)
]

{ #category : #accessing }
GLMRubricTextPresentation >> tabWidth [
	^ tabWidth ifNil: [ TextStyle default rubTabWidth ]
]

{ #category : #accessing }
GLMRubricTextPresentation >> tabWidth: anInteger [
	tabWidth := anInteger
]

{ #category : #accessing }
GLMRubricTextPresentation >> textSegments [
	^textSegments ifNil: [ textSegments := OrderedCollection new ]
]

{ #category : #accessing }
GLMRubricTextPresentation >> withAnnotation [
	
	^ withAnnotation ifNil: [ withAnnotation := false ]
]

{ #category : #accessing }
GLMRubricTextPresentation >> withAnnotation: aBoolean [
	
	withAnnotation := aBoolean
]

{ #category : #accessing }
GLMRubricTextPresentation >> withColumns [
	
	^ withColumns ifNil: [ withColumns := false ]
]

{ #category : #accessing }
GLMRubricTextPresentation >> withColumns: aBoolean [
	
	withColumns := aBoolean
]

{ #category : #accessing }
GLMRubricTextPresentation >> withGhostText [
	
	^ withGhostText
]

{ #category : #accessing }
GLMRubricTextPresentation >> withGhostText: aText [
	
	withGhostText := aText
]

{ #category : #accessing }
GLMRubricTextPresentation >> withLineNumbers [
	
	^ withLineNumbers ifNil: [ withLineNumbers := false ]
]

{ #category : #accessing }
GLMRubricTextPresentation >> withLineNumbers: aBoolean [
	
	withLineNumbers := aBoolean
]

{ #category : #accessing }
GLMRubricTextPresentation >> withTextSegments: aCollection [
	self textSegments addAll: aCollection
]

{ #category : #accessing }
GLMRubricTextPresentation >> wrapped [
	
	^  wrapped ifNil: [ wrapped := true ]
]

{ #category : #accessing }
GLMRubricTextPresentation >> wrapped: aBoolean [
	
	wrapped := aBoolean
]
