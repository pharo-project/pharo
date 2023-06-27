Class {
	#name : #RBRemoveInstanceVariableDriver,
	#superclass : #RBDriver,
	#instVars : [
		'class',
		'variables'
	],
	#category : #'Refactoring-UI'
}

{ #category : #execution }
RBRemoveInstanceVariableDriver >> execute [

	| changes refactoring |
	refactoring := self refactoring.
	changes := [
	           [ self halt. refactoring generateChanges ]
		           on: RBApplicabilityChecksFailedError
		           do: [ :err |
		           ^ RBRefactoringError signal: err messageText ] ]
		           on: RBBreakingChangeChecksFailedWarning
		           do: [ :err |
			
								self halt.
			           RBRefactoringWarning 
									signal: err messageText 
									with: [ self halt. self furtherActionFor: (RBInstanceVariableStillReferenced new refactoring: refactoring) ].
			           "If user answers no, error is being propagated."
			           err resume ].
	self openPreviewWithChanges: changes
]

{ #category : #execution }
RBRemoveInstanceVariableDriver >> furtherActionFor: aReport [

	^ (aReport refactoring options at: #openBrowser)
			value: aReport refactoring 
			value: aReport environment
]

{ #category : #resources }
RBRemoveInstanceVariableDriver >> refactoring [

	^ RBRemoveInstanceVariableRefactoring2 
		  model: model remove: variables first from: class
]

{ #category : #initialization }
RBRemoveInstanceVariableDriver >> scopes: refactoringScopes variable: aVariable for: aClass [
	
	scopes := refactoringScopes.
	variables := {aVariable}.
	class := aClass
]

{ #category : #resources }
RBRemoveInstanceVariableDriver >> selectVariables [

	| classes |
	classes := class withAllSubclasses 
					removeAllSuchThat: [ :each |
		           { Object . ProtoObject . Class . Behavior. Metaclass . ClassDescription} includes: each ].
	
	variables := StVariablesSelectionPresenter
		label: 'Select one instance variable (for now) to remove'
		withItems: class instVarNames
		selecting: variables asOrderedCollection
		
]
