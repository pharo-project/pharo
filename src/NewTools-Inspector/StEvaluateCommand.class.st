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

{ #category : #executing }
StEvaluateCommand >> execute [
	
	self context doEvaluateAndGo
]
