"
I am a refactoring for extracting code fragments to a new method. 

Similar to `RBExtractMethodRefactoring`, but you can choose to which component (instance or agument variable) the new method is added. 
As such, the new method arguments will include an additional argument for the sender.
Based on the instance variable you chosed for this method I will guess the class where to add this method, but you can change this class or add more classes.

"
Class {
	#name : 'RBExtractMethodToComponentRefactoring',
	#superclass : 'RBMethodRefactoring',
	#instVars : [
		'selector',
		'extractionInterval',
		'extractedMethodSelector',
		'selfArgumentName'
	],
	#category : 'Refactoring-Core-Refactorings-Unused',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings-Unused'
}

{ #category : 'instance creation' }
RBExtractMethodToComponentRefactoring class >> extract: anInterval from: aSelector in: aClass [
	^ self new
		extract: anInterval
		from: aSelector
		in: aClass
]

{ #category : 'instance creation' }
RBExtractMethodToComponentRefactoring class >> model: aRBSmalltalk extract: anInterval from: aSelector in: aClass [
	^ self new
		model: aRBSmalltalk;
		extract: anInterval
			from: aSelector
			in: aClass;
		yourself
]

{ #category : 'initialization' }
RBExtractMethodToComponentRefactoring >> extract: anInterval from: aSelector in: aClass [
	class := self classObjectFor: aClass.
	selector := aSelector.
	extractionInterval := anInterval
]

{ #category : 'transforming' }
RBExtractMethodToComponentRefactoring >> extractMethod [
	| refactoring |
	refactoring := RBExtractMethodRefactoring
				model: self model
				extract: extractionInterval
				from: selector
				in: class.
	refactoring setOption: #methodName
		toUse:
			[:methodName :ref |
			extractedMethodSelector := self
						uniqueMethodNameFor: methodName arguments size.
			methodName
				selector: extractedMethodSelector;
				yourself].
	self generateChangesFor: refactoring
]

{ #category : 'transforming' }
RBExtractMethodToComponentRefactoring >> inlineForwarder [
	| refactoring |
	refactoring := ReInlineAllSendersRefactoring
				model: self model
				sendersOf: extractedMethodSelector
				in: class.
	refactoring setOption: #shouldNotCreateExtraBindings toUse: [:ref :string | true].
	self generateChangesFor: refactoring
]

{ #category : 'transforming' }
RBExtractMethodToComponentRefactoring >> moveMethod [

	| variable refactoring |

	variable := self selectVariableToMoveMethodTo: extractedMethodSelector class: class.
	variable ifNil: [ self refactoringError: 'Did not extract method' ].
	refactoring := RBMoveMethodRefactoring
		model: self model
		selector: extractedMethodSelector
		class: class
		variable: variable.
	refactoring selfArgumentName: selfArgumentName.
	self generateChangesFor: refactoring
]

{ #category : 'transforming' }
RBExtractMethodToComponentRefactoring >> privateTransform [
	self
		extractMethod;
		moveMethod;
		inlineForwarder
]

{ #category : 'transforming' }
RBExtractMethodToComponentRefactoring >> selfArgumentName: aString [ 
	selfArgumentName := aString
]
