Class {
	#name : 'RubCursorAction',
	#superclass : 'Object',
	#instVars : [
		'cmdPressed',
		'optionPressed'
	],
	#category : 'Rubric-Editing-Core',
	#package : 'Rubric',
	#tag : 'Editing-Core'
}

{ #category : 'constructor' }
RubCursorAction class >> cmdPressed: cmdPressed optPressed: optPressed [

	^ self new cmd: cmdPressed; opt: optPressed
]

{ #category : 'initialization' }
RubCursorAction >> cmd: pressed [

	cmdPressed := pressed 
]

{ #category : 'initialization' }
RubCursorAction >> initialize [ 

	super initialize.

]

{ #category : 'movement' }
RubCursorAction >> isForward [
	
	^ self subclassResponsibility 
]

{ #category : 'movement' }
RubCursorAction >> moveFromPosition: position inEditor: editor [
	self subclassResponsibility.
]

{ #category : 'initialization' }
RubCursorAction >> opt: pressed [

	optionPressed := pressed
]
