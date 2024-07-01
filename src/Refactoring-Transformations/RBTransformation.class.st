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
	#name : 'RBTransformation',
	#superclass : 'RBAbstractTransformation',
	#category : 'Refactoring-Transformations-Model',
	#package : 'Refactoring-Transformations',
	#tag : 'Model'
}

{ #category : 'private - initialization' }
RBTransformation class >> initializeRefactoringOptions [

	RefactoringOptions := IdentityDictionary new.
	RefactoringOptions
		at: #implementorToInline
		put: [ :ref :imps | self error: #implementorToInline ];

		at: #methodName
		put: [ :methodName | self error: #methodName ];

		at: #selfArgumentName
		put: [ :ref | self error: #selfArgumentName ];

		at: #selectVariableToMoveTo
		put: [ :ref :class :selector | self error: #selectVariableToMoveTo ];

		at: #variableTypes
		put: [ :ref :types :selected | self error: #variableTypes ];

		at: #extractAssignment
		put: [ :ref :varName | self error: #extractAssignment ];

		at: #shouldNotCreateExtraBindings
		put: [ :ref :string | self error: #shouldNotCreateExtraBindings ];

		at: #alreadyDefined
		put: [ :ref :cls :selector |  self error: #alreadyDefined ];

		at: #openBrowser
		put: [ :ref :env |  self error: #openBrowser ]
]

{ #category : 'converting' }
RBTransformation >> asRefactoring [

	self deprecated: 'This method is a no-op and can be removed' transformWith: '`@rec asRefactoring' -> '`@rec'.
	^ self yourself
]

{ #category : 'transforming' }
RBTransformation >> transform [

	self deprecated: 'Use generateChanges instead' transformWith: '`@rec transform' -> '`@rec generateChanges'.
	
	"Performs this transformation in a model,
	 then it adds the refactoring to the manager"

	self checkPreconditions.
	self privateTransform

	"below executes the refactoring without prompt"
	"RBRefactoringManager instance addRefactoring: self"
]

{ #category : 'displaying' }
RBTransformation >> whatToDisplayIn: aBrowser [

	^ self changes changes
		flatCollect: [ :e | e whatToDisplayIn: aBrowser ]
]
