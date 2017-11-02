Class {
	#name : #GLMRubricTextModel,
	#superclass : #GLMMorphicModel,
	#instVars : [
		'primarySelectionInterval',
		'hasUnacceptedEdits'
	],
	#category : 'Glamour-Morphic-Widgets'
}

{ #category : #'text managing' }
GLMRubricTextModel >> clearUserEdits [
	
	self hasUnacceptedEdits: false
]

{ #category : #accessing }
GLMRubricTextModel >> getText [
	^ self glamourPresentation formatedDisplayValue
]

{ #category : #accessing }
GLMRubricTextModel >> hasUnacceptedEdits [
	^ hasUnacceptedEdits ifNil: [ hasUnacceptedEdits := false ]
]

{ #category : #accessing }
GLMRubricTextModel >> hasUnacceptedEdits: aBoolean [
	hasUnacceptedEdits := aBoolean.
	self announce: RubEditsStateChangedInModel new
]

{ #category : #callbacks }
GLMRubricTextModel >> menu [

	^ self menu: MenuMorph new
]

{ #category : #accessing }
GLMRubricTextModel >> primarySelectionInterval [
	^ primarySelectionInterval
]

{ #category : #accessing }
GLMRubricTextModel >> primarySelectionInterval: anInterval [ 
	primarySelectionInterval := anInterval
]

{ #category : #accessing }
GLMRubricTextModel >> setText: aText from: aRubScrolledTextMorph [ 
	self glamourPresentation text: aText.
	^true
]

{ #category : #callbacks }
GLMRubricTextModel >> shiftMenu [

	^ nil
]
