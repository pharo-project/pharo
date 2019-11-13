"
This is the class responsible for the binding to Morphic.

| browser |
browser := GLMTabulator new.
browser column: #one; column: #two.

browser transmit to: #one; andShow: [:a | a list.].
browser transmit to: #two; from: #one; andShow: [ :a |
	a text.].
browser openOn: #(a b c d)
"
Class {
	#name : #GLMMorphicRenderer,
	#superclass : #GLMRenderer,
	#instVars : [
		'window'
	],
	#category : #'Glamour-Morphic-Renderer'
}

{ #category : #opening }
GLMMorphicRenderer >> open: aRenderable [
	window := GLMMorphicWindowRenderer render: aRenderable from: self.
	^ window openInWorld 
]

{ #category : #OSWindow }
GLMMorphicRenderer >> openInExternalWindow: aRenderable [
    window := GLMOSWindowMorphWorldRenderer render: aRenderable from: self.
    ^ window open
]

{ #category : #'rendering presentations' }
GLMMorphicRenderer >> renderAccordionCompositePresentation: aCompositePresentation [
	^ GLMMorphicAccordionRenderer render: aCompositePresentation from: self
]

{ #category : #'rendering actions' }
GLMMorphicRenderer >> renderAction: anAction [
	^ GLMMorphicActionRenderer render: anAction from: self
]

{ #category : #'rendering presentations' }
GLMMorphicRenderer >> renderActionListPresentation: anActionListPresentation [ 
	^GLMMorphicActionListRenderer render: anActionListPresentation from: self
]

{ #category : #'rendering presentations' }
GLMMorphicRenderer >> renderDiffPresentation: aPresentation [
	^ GLMMorphicDiffRenderer render: aPresentation from: self
]

{ #category : #'rendering presentations' }
GLMMorphicRenderer >> renderDropDownListPresentation: aDropListPresentation [
	^ GLMMorphicDropDownRenderer render: aDropListPresentation from: self	
]

{ #category : #'rendering presentations' }
GLMMorphicRenderer >> renderDynamicPresentation: aPresentation [
	^ GLMMorphicDynamicRenderer render: aPresentation from: self
]

{ #category : #'rendering browsers' }
GLMMorphicRenderer >> renderExpander: aBrowser [ 
	^ GLMMorphicExpanderRenderer render: aBrowser from: self
]

{ #category : #'rendering browsers' }
GLMMorphicRenderer >> renderFinder: aBrowser [ 
	^ aBrowser hasFixedSizePanes 
		ifTrue: [GLMMorphicFixedPanesFinderRenderer render: aBrowser from: self]
		ifFalse: [GLMMorphicVariablePanesFinderRenderer render: aBrowser from: self]
]

{ #category : #'rendering presentations' }
GLMMorphicRenderer >> renderLabelPresentation: aLabelPresentation [ 
	^ GLMMorphicLabelRenderer render: aLabelPresentation from: self
]

{ #category : #'rendering presentations' }
GLMMorphicRenderer >> renderListPresentation: aListPresentation [ 
	^ GLMMorphicListRenderer render: aListPresentation from: self
]

{ #category : #'rendering presentations' }
GLMMorphicRenderer >> renderListingPresentation: aPresentation [ 
	^ GLMMorphicListingRenderer render: aPresentation from: self
]

{ #category : #'rendering presentations' }
GLMMorphicRenderer >> renderMorphPresentation: aMorphPresentation [ 
	^ GLMMorphicMorphRenderer render: aMorphPresentation from: self
]

{ #category : #rendering }
GLMMorphicRenderer >> renderPane: aPane [ 
	^ GLMMorphicPaneRenderer render: aPane from: self
]

{ #category : #'rendering presentations' }
GLMMorphicRenderer >> renderPharoMethodPresentation: aSmalltalkMethodPresentation [ 
	^ GLMMorphicPharoMethodRenderer render: aSmalltalkMethodPresentation from: self
]

{ #category : #'rendering presentations' }
GLMMorphicRenderer >> renderPharoScriptPresentation: aPharoScriptPresentation [ 
	^ GLMMorphicPharoScriptRenderer render: aPharoScriptPresentation from: self
]

{ #category : #'rendering actions' }
GLMMorphicRenderer >> renderPopupAction: anAction [
	^ GLMMorphicPopupActionRenderer render: anAction from: self
]

{ #category : #'rendering presentations' }
GLMMorphicRenderer >> renderRubricHighlightedTextPresentation: aPresentation [

	^ GLMMorphicRubricHighlightedTextRenderer render: aPresentation from: self
]

{ #category : #'rendering presentations' }
GLMMorphicRenderer >> renderRubricTextPresentation: aPresentation [
	^ GLMMorphicRubricTextRenderer render: aPresentation from: self
]

{ #category : #'rendering presentations' }
GLMMorphicRenderer >> renderSmalltalkCodePresentation: aSmalltalkCodePresentation [ 
	^ GLMMorphicSmalltalkCodeRenderer render: aSmalltalkCodePresentation from: self
]

{ #category : #'rendering presentations' }
GLMMorphicRenderer >> renderSpecPresentation: aPresentation [ 
	^ GLMMorphicSpecRenderer render: aPresentation from: self
]

{ #category : #'rendering presentations' }
GLMMorphicRenderer >> renderStackedVerticallyCompositePresentation: aCompositePresentation [
	^ GLMMorphicVerticallyStackedRenderer render: aCompositePresentation from: self
]

{ #category : #'rendering browsers' }
GLMMorphicRenderer >> renderStacker: aBrowser [ 
	^ GLMMorphicStackerRenderer render: aBrowser from: self
]

{ #category : #'rendering presentations' }
GLMMorphicRenderer >> renderTabbedCompositePresentation: aCompositePresentation [
	^ GLMMorphicTabbedRenderer render: aCompositePresentation from: self
]

{ #category : #'rendering presentations' }
GLMMorphicRenderer >> renderTablePresentation: aPresentation [ 
	^ GLMMorphicTableRenderer render: aPresentation from: self
]

{ #category : #'rendering browsers' }
GLMMorphicRenderer >> renderTabulator: aBrowser [ 
	^ GLMMorphicTabulatorRenderer render: aBrowser from: self
]

{ #category : #'rendering presentations' }
GLMMorphicRenderer >> renderTextPresentation: aTextPresentation [ 
	^ GLMMorphicTextRenderer render: aTextPresentation from: self
]

{ #category : #'rendering presentations' }
GLMMorphicRenderer >> renderTreePresentation: aPresentation [ 
	^ GLMMorphicTreeRenderer render: aPresentation from: self
]

{ #category : #accessing }
GLMMorphicRenderer >> window [
	^ window
]

{ #category : #accessing }
GLMMorphicRenderer >> window: anObject [
	window := anObject
]
