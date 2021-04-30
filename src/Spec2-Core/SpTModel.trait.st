"
A trait to add ""model"" behavior to a presenter.
"
Trait {
	#name : #SpTModel,
	#instVars : [
		'model'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #transmission }
SpTModel >> defaultInputPort [

	^ self inputModelPort
]

{ #category : #transmission }
SpTModel >> inputModelPort [

	^ SpModelPort newPresenter: self
]

{ #category : #'accessing model' }
SpTModel >> model [

	^ model
]

{ #category : #'accessing model' }
SpTModel >> model: aModel [

	model := aModel.
	self updatePresenter
]

{ #category : #private }
SpTModel >> setModel: aModel [

	self model: aModel
]

{ #category : #'accessing model' }
SpTModel >> setModelBeforeInitialization: aModel [

	model := aModel
]
