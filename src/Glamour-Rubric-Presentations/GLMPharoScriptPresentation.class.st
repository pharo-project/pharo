"
I provide support for displaying snippets of Smalltalk code  (e.g. dealing with variable binding)

"
Class {
	#name : #GLMPharoScriptPresentation,
	#superclass : #GLMRubricSmalltalkCodePresentation,
	#category : #'Glamour-Rubric-Presentations'
}

{ #category : #examples }
GLMPharoScriptPresentation class >> pharoScriptPresentationExample [
	"
	self pharoScriptPresentationExample
	"
	| browser |
	browser := GLMTabulator new.
	browser
		row: #r1;
		row: #r2.
	browser transmit
		to: #r1;
		andShow: [ :aComposite | 
					aComposite pharoScript
						selectionAct: [ :workspace :page | workspace evaluateSelection ] on: $d entitled: 'Do it';
						selectionAct: [ :workspace :page | workspace evaluateSelectionAndDo: [ :result | result inspect ] ]
							on: $i
							entitled: 'Inspect it'].
	browser transmit
		to: #r2;
		andShow: [ :aComposite | 
					aComposite pharoScript
						variableBindings: [ 
							{(#browser -> browser).
							(#a -> 1)} ] ].
	browser
		openOn:
			'browser := GLMTabulator new.
browser row: #r1; row: #r2.

browser transmit to: #r1; andShow: [ :aComposite |
	aComposite pharoPlayground].
browser openOn:''self halt'' '
]

{ #category : #accessing }
GLMPharoScriptPresentation >> currentBindings [

	self withBindingsDo: [ :bingings | ^ bingings copy ]
]

{ #category : #accessing }
GLMPharoScriptPresentation >> defaultSelectionActions [

	^ {self goAction}, super defaultSelectionActions
]

{ #category : #actions }
GLMPharoScriptPresentation >> goAction [
	^ GLMGenericAction new
				title: 'Do it and go';
				action: [ :t :entity |
					t highlightEvaluateAndDo: [ :result | t selection: result ] ]; 
				shortcut: $g; 
				yourself.
]

{ #category : #rendering }
GLMPharoScriptPresentation >> renderGlamorouslyOn: aRenderer [ 
	self registerAnnouncements.
	^ aRenderer renderPharoScriptPresentation: self
]

{ #category : #actions }
GLMPharoScriptPresentation >> resetBindings [

	self withBindingsDo: [ :bindings | bindings removeAll ]
]

{ #category : #actions }
GLMPharoScriptPresentation >> updateVariableBindings [

	self announce: (GLMVariableBindingsChanged new presentation: self).
]

{ #category : #actions }
GLMPharoScriptPresentation >> withBindingsDo: aBlock [

	self announce: (GLMVariableBindingsAccessRequest new action: aBlock)
]
