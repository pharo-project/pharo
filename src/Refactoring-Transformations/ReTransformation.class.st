"
I represent an elementary code transformation.
My subinstances can be composed into composite transformations. 

I'm not a refactoring since a transformation I do not pay attention to behavior preservation. 
However as a transformation, I will be used by refactorings. 

My main API is 
	- transform which is actually executing the transformation
	- asRefactoring to optionally (false by default) check preconditions, if some apply

"
Class {
	#name : 'ReTransformation',
	#superclass : 'ReAbstractTransformation',
	#category : 'Refactoring-Transformations-Model',
	#package : 'Refactoring-Transformations',
	#tag : 'Model'
}

{ #category : 'printing' }
ReTransformation >> changesToDisplayIn: aBrowser [

	^ self changes changes
		flatCollect: [ :e | e changesToDisplayIn: aBrowser ]
]

{ #category : 'preconditions' }
ReTransformation >> failedBreakingChangePreconditions [
	"Return an empty collection since transformations don't have behavior preserving preconditions"

	^ {  } 
]
