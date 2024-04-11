Class {
	#name : 'ReCompositeExtractSetUpMethodRefactoring',
	#superclass : 'ReCompositeExtractMethodRefactoring',
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'execution' }
ReCompositeExtractSetUpMethodRefactoring >> addReturnIfNeeded: aMethodNode [

	"setUp don't return a value"
]

{ #category : 'preconditions' }
ReCompositeExtractSetUpMethodRefactoring >> applicabilityPreconditions [

	^ self preconditionParsableSubtree
	  & self preconditionNotInCascadedMessage
	  & self preconditionTemporariesAreNotReadBeforeWritten
	  & self preconditionAssignmentsNotReadBeforeWritten
	  & self preconditionSubtreeDoesNotContainsReturn
	  & (RBCondition definesSelector: #setUp in: class) not "we can remove here since add override method checks it"
	  & (RBCondition withBlock: [class allSuperclasses anySatisfy: [ :e | e name = #TestCase ]])
]

{ #category : 'executing' }
ReCompositeExtractSetUpMethodRefactoring >> buildTransformationFor: newMethodName [

	| messageSend |
	messageSend := self messageSendWith: newMethodName.

	^ OrderedCollection new
		  add: (RBAddMethodTransformation 
				   model: self model
				   sourceCode: newMethod newSource
				   in: class
				   withProtocol: (Protocol named: #running));
		  add: (ReAddSuperSendAsFirstStatementTransformation 
					model: self model
					methodTree: newMethod
					inClass: class);
		  add: (RBRemoveSubtreeTransformation
				   model: self model
				   remove: sourceCode
				   fromMethod: selector
				   inClass: class);
		  add: (ReRemoveUnusedTemporaryVariableRefactoring
				   model: self model
				   inMethod: selector
				   inClass: class name);
		  yourself
]

{ #category : 'instance creation' }
ReCompositeExtractSetUpMethodRefactoring >> extractSetUpCode: sourceCodeString fromMethod: aSelector inClass: aClassName [ 
	
	sourceCode := sourceCodeString.
	selector := aSelector.
	class := self model classNamed: aClassName.
	newSelector := #setUp
]
