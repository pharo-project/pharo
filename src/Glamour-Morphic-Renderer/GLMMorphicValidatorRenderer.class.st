Class {
	#name : #GLMMorphicValidatorRenderer,
	#superclass : #GLMMorphicBrowserRenderer,
	#instVars : [
		'container'
	],
	#category : #'Glamour-Morphic-Renderer'
}

{ #category : #rendering }
GLMMorphicValidatorRenderer >> render: aBrowser [
	| tmpButtons |
	tmpButtons := OrderedCollection new.
	aBrowser panes isEmpty
		ifTrue: [ 
			"|tmpPane|
		tmpPane := (GLMPane in: aBrowser) 
				addPresentation: aBrowser presentations; 
				yourself.	
		(tmpPane port: #entity) value: aBrowser entity.	
		aBrowser panes add: tmpPane"
			 ].
	aBrowser buttonActions
		do: [ :aGLMAction | 
			| buttonModel |
			buttonModel := GLMButtonModel new.
			buttonModel glamourPresentation: aBrowser.
			buttonModel glamourAction: aGLMAction.
			tmpButtons
				add:
					(PluggableButtonMorph
						on: buttonModel
						getState: nil
						action: #execute
						label: #buttonLabel) ].
	aBrowser when: GLMBrowserClosed send: #actOnBrowserClosed: to: self.
	container := GLMMorphicValidatorPaneRenderer new
		buttons: tmpButtons;
		render: aBrowser panes anyOne.
	^ container
]

{ #category : #rendering }
GLMMorphicValidatorRenderer >> render: aBrowser from: aGLMRenderer [

	self renderer: aGLMRenderer.
	^ self render: aBrowser
	
]
