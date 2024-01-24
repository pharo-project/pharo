"
I am a refactoring for moving a class variable down to my subclasses.

My precondition verifies that the moved variable is not referenced in the methods of the original class.
"
Class {
	#name : 'RBPushDownClassVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#instVars : [
		'destinationClass'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'preconditions' }
RBPushDownClassVariableRefactoring >> applicabilityPreconditions [ 

	^ RBCondition definesClassVariable: variableName in: class
]

{ #category : 'preconditions' }
RBPushDownClassVariableRefactoring >> breakingChangePreconditions [
	"Preconditions are that only one subclass refers to the class variable."

	^ RBCondition withBlock: [
		  self findDestinationClass.
		  true ]
]

{ #category : 'preconditions' }
RBPushDownClassVariableRefactoring >> findDestinationClass [
	| classes |
	classes := class withAllSubclasses reject: [ :each |
		(each whichSelectorsReferToClassVariable: variableName) isEmpty
			and: [ (each classSide whichSelectorsReferToClassVariable: variableName) isEmpty ] ].
	destinationClass := classes ifEmpty: [ nil ]
		ifNotEmpty: [ classes asOrderedCollection first ].
	classes do: [ :each |
		(destinationClass includesClass: each)
			ifTrue: [ destinationClass := each ]
				ifFalse: [
					(each includesClass: destinationClass)
						ifFalse: [ self signalMultipleReferenceError ] ] ].
	destinationClass = class
		ifTrue: [ self signalStillReferencedError ].
	^ destinationClass
]

{ #category : 'preconditions' }
RBPushDownClassVariableRefactoring >> preconditions [

	^ self applicabilityPreconditions & self breakingChangePreconditions
]

{ #category : 'transforming' }
RBPushDownClassVariableRefactoring >> privateTransform [

	class removeClassVariable: variableName.
	destinationClass ifNil: [ ^ self ].
	destinationClass addClassVariable: variableName
]

{ #category : 'preconditions' }
RBPushDownClassVariableRefactoring >> signalMultipleReferenceError [
	self signalReferenceError: ('Multiple subclasses reference <1s>'
				expandMacrosWith: variableName)
]

{ #category : 'preconditions' }
RBPushDownClassVariableRefactoring >> signalReferenceError: errorString [

	class realClass
		ifNil: [ self refactoringError: errorString ]
		ifNotNil: [ | classVarName error |

			error := '<1s><n>Browse references?' expandMacrosWith: errorString.
			classVarName := variableName asSymbol.
			self
				refactoringWarning: error
				with: [ self
						openBrowserOn: ( RBVariableEnvironment referencesToClassVariable: classVarName in: class realClass )
					]
			]
]

{ #category : 'preconditions' }
RBPushDownClassVariableRefactoring >> signalStillReferencedError [
	self signalReferenceError: ('<1p> has references to <2s>'
				expandMacrosWith: class
				with: variableName)
]
