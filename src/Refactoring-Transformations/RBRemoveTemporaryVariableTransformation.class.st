"
RBRemoveTemporaryVariableRefactoring is responsible for removing temporary variables inside a method. In Pharo, temporary variables can be declared inside blocks as well.

Example:
```
| transformation |
transformation := (RBRemoveTemporaryVariableTransformation
	variable: 'methodTree'
	inMethod: #privateTransform
	inClass: #RBRemoveTemporaryVariableTransformation)
			generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```
	
Preconditions:
- Check whether this method has a temporary variable with the given name.

Observations about the transformation:
- The transformation looks for the variable inside blocks too
"
Class {
	#name : 'RBRemoveTemporaryVariableTransformation',
	#superclass : 'RBMethodTransformation',
	#instVars : [
		'variableName'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'api' }
RBRemoveTemporaryVariableTransformation class >> model: aRBModel variable: aString inMethod: aSelector inClass: aClass [

	^ self new
		model: aRBModel;
		variable: aString
		inMethod: aSelector
		inClass: aClass;
		yourself
]

{ #category : 'api' }
RBRemoveTemporaryVariableTransformation class >> variable: aString inMethod: aSelector inClass: aClass [

	^ self new
		variable: aString
		inMethod: aSelector
		inClass: aClass;
		yourself
]

{ #category : 'preconditions' }
RBRemoveTemporaryVariableTransformation >> applicabilityPreconditions [

	^ { 
		self classExist.
		(ReDefinesSelectorsCondition new definesSelectors: { selector } in: self definingClass).
		(RBCondition
			withBlock: [ 
				self definingMethod allTemporaryVariables includes: variableName ]
			errorString: 'Method named ' , selector
				, ' does not define a temporary variable named ' , variableName).
		(RBCondition
			withBlock: [ 
				(self definingMethod argumentNames includes: variableName) not ]
			errorString: 'Variable named ' , variableName
				, ' cannot be removed because it is an argument in this method').
		(RBCondition
			withBlock: [ 
				self hasUsagesInScope not or: [ 
					self definingClass definesInstanceVariable: variableName ] ]
			errorString: 'Variable named ' , variableName
				, ' cannot be removed because it is used in the method') }
]

{ #category : 'scripting api - conditions' }
RBRemoveTemporaryVariableTransformation >> checkPreconditions [ 

	self eagerlyCheckApplicabilityPreconditions 
]

{ #category : 'preconditions' }
RBRemoveTemporaryVariableTransformation >> classExist [

	^ RBCondition
		  withBlock: [ self definingClass isNotNil ]
		  errorString: 'No such class or trait named ' , className asString
]

{ #category : 'private - accessing' }
RBRemoveTemporaryVariableTransformation >> definingSequenceNode [
	"Answer the sequence whose declaration of the variable is removed, or nil when none declares it.
	When several scopes declare the same name (a block temporary shadowing an outer one), prefer one
	where the variable is unused: nothing refers to that declaration, so it can be removed safely."

	| candidates |
	candidates := self definingMethod allChildren select: [ :each | each isSequence and: [ each exactNodeDefines: variableName ] ].
	candidates ifEmpty: [ ^ nil ].
	^ candidates detect: [ :each | (self hasUsagesIn: each) not ] ifNone: [ candidates first ]
]

{ #category : 'private-testing' }
RBRemoveTemporaryVariableTransformation >> hasUsagesIn: aSequenceNode [
	"Answer whether a variable node refers to the declaration of the variable in aSequenceNode,
	and not to another declaration of the same name in an inner scope."

	^ aSequenceNode statements anySatisfy: [ :statement |
			  statement withAllChildren anySatisfy: [ :node |
				  node isVariable and: [ node name = variableName and: [ (node whichUpNodeDefines: variableName) == aSequenceNode ] ] ] ]
]

{ #category : 'private-testing' }
RBRemoveTemporaryVariableTransformation >> hasUsagesInScope [

	| definingSequence |
	definingSequence := self definingSequenceNode.
	definingSequence ifNil: [ ^ false ].
	^ self hasUsagesIn: definingSequence
]

{ #category : 'transforming' }
RBRemoveTemporaryVariableTransformation >> privateTransform [

	self definingSequenceNode ifNotNil: [ :sequence | sequence removeTemporaryNamed: variableName ].
	class compileTree: self definingMethod
]

{ #category : 'api' }
RBRemoveTemporaryVariableTransformation >> variable: aVariableName inMethod: aSelector inClass: aClass [

	self className: aClass.
	selector := aSelector.
	variableName := aVariableName
]
