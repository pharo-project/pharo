"
I keep the context of the ocompletion. 
I do the same as my parent but for this specific algorithm
"
Class {
	#name : #NOCContext,
	#superclass : #NECContext,
	#category : #'NECompletion-OCompletion'
}

{ #category : #'model creation' }
NOCContext >> createModel [
	| aModel |
	
	aModel := NOCModel
		class: theClass
		temporaries: self temporaries
		additionals: controller additionals
		variables: variables
		selectors: selectors.
	aModel
		context: self.
		
	^ aModel 
]

{ #category : #'model creation' }
NOCContext >> nextModel [ 
	"Provide a next lookup model. 
	 It is defining a chain of responsibility, translating the model to eCompletion, to ensure having results"
	^ super createModel
]
