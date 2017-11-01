Class {
	#name : #GLMMorphicWizardRenderer,
	#superclass : #GLMMorphicBrowserRenderer,
	#instVars : [
		'scrollPane',
		'container',
		'parentRenderer'
	],
	#category : 'Glamour-Morphic-Renderer'
}

{ #category : #rendering }
GLMMorphicWizardRenderer >> actOnPaneAdded: ann [
	container removeMorph: container submorphs last.
	self addMorphFromObject: ann pane toContainer: container.
]

{ #category : #rendering }
GLMMorphicWizardRenderer >> actOnPaneRemoved: ann [
]

{ #category : #rendering }
GLMMorphicWizardRenderer >> actOnPaneReplaced: ann [

	container removeMorph: container submorphs last.
	self addMorphFromObject: ann newPane toContainer: container.
	"scrollPane hScrollbar glmAnimateValue: 1.0 duration: 200"
]

{ #category : #rendering }
GLMMorphicWizardRenderer >> addMorphFromObject: anObject toContainer: aContainerMorph [

	| renderedMorph |
	renderedMorph := (self renderObject: anObject).
				"vResizing: #spaceFill;
				hResizing: #rigid;
				minimumExtent: 200@10;
				extent: 200@10;
				yourself."
	aContainerMorph addMorphBack: renderedMorph.
]

{ #category : #accessing }
GLMMorphicWizardRenderer >> parentRenderer [

	^ parentRenderer
]

{ #category : #accessing }
GLMMorphicWizardRenderer >> parentRenderer: aGLMMorphicRenderer [

	parentRenderer := aGLMMorphicRenderer
]

{ #category : #rendering }
GLMMorphicWizardRenderer >> registerAnnouncementsFor: aBrowser [
	aBrowser when: GLMPaneRemoved send: #actOnPaneRemoved: to: self.
	aBrowser when: GLMPaneAdded send: #actOnPaneAdded: to: self.
	aBrowser when: GLMPaneReplaced send: #actOnPaneReplaced: to: self.
	aBrowser when: GLMBrowserClosed send: #actOnBrowserClosed: to: self
]

{ #category : #rendering }
GLMMorphicWizardRenderer >> render: aBrowser [
	container := GLMMorphic containerMorph.
	container
		changeTableLayout;
		listDirection: #leftToRight;
		vResizing: #spaceFill;
		hResizing: #spaceFill.
	scrollPane := GeneralScrollPane new
		changeScrollerTableLayout;
		scrollTarget: container;
		yourself.
	self registerAnnouncementsFor: aBrowser.  
	self addMorphFromObject: (aBrowser stepsAndPanes at: aBrowser currentStep) last toContainer: container .
	^ scrollPane
]
