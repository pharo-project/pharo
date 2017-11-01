"
This trait contains the necessary methods to manipulate correctly a RubScrolledTextMorph via a model.
Should be use in the model class of the architecture. 

The interactionModel can represents your model or a model of the higher level (AbstractNautilusUI for the text area of Nautilus for example).

"
Trait {
	#name : #TViewModel,
	#category : 'Rubric-Editing-Core-Support'
}

{ #category : #accessing }
TViewModel >> announcer [
	"Define #announcer if needed"
]

{ #category : #accessing }
TViewModel >> interactionModel [
	^ self
]
