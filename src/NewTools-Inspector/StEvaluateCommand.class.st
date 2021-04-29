"
A command to ""evaluate and go"" the content of a selection (evaluate the selection and open an inspector with the results).

"
Class {
	#name : #StEvaluateCommand,
	#superclass : #SpToolCommand,
	#category : #'NewTools-Inspector-Command'
}

{ #category : #default }
StEvaluateCommand class >> defaultDescription [

	^ 'Evaluate selection and open an inspection column.'
]

{ #category : #default }
StEvaluateCommand class >> defaultIconName [

	^ #glamorousGo
]

{ #category : #default }
StEvaluateCommand class >> defaultName [

	^ 'Do it and go'
]

{ #category : #default }
StEvaluateCommand class >> defaultShortcutKey [

	^ $g command mac 
		| $g control win
		| $g control unix
]

{ #category : #documentation }
StEvaluateCommand class >> documentContextRequiredSelectors [

	^ super documentContextRequiredSelectors, #(doEvaluateAndGo)
]

{ #category : #executing }
StEvaluateCommand >> execute [
	
	self context doEvaluateAndGo
]
