"
I am an atomic item used to execute a portion of code at image startup.
"
Class {
	#name : #StartupAction,
	#superclass : #Object,
	#instVars : [
		'code',
		'hasBeenExecuted',
		'runOnce',
		'name'
	],
	#category : #StartupPreferences
}

{ #category : #'instance creation' }
StartupAction class >> name: nameOfItem code: code [

	^ self new
		code: code;
		name: nameOfItem;
		yourself
]

{ #category : #'instance creation' }
StartupAction class >> name: nameOfItem code: code runOnce: aBoolean [

	^ self new
		code: code;
		name: nameOfItem;
		runOnce: aBoolean;
		yourself
]

{ #category : #comparing }
StartupAction >> = anObject [

	anObject class = self class ifFalse: [ ^ false ].
	^ self code = anObject code and: [ self name = anObject name and: [ self runOnce = anObject runOnce ]]
]

{ #category : #accessing }
StartupAction >> code [

	^ code
]

{ #category : #accessing }
StartupAction >> code: anObject [
	
	code := anObject
]

{ #category : #execution }
StartupAction >> execute [

	| block correctlyExecuted |
	(self hasBeenExecuted and: [ self runOnce ]) ifTrue: [ ^ self ].
	block := self code isBlock
				ifTrue: [ self code ]
				ifFalse: [[ self class compiler evaluate: self code ]].
	correctlyExecuted := true. 
	block on: Halt, Error do: [ :ex | 
		StartupPreferencesLoader default errors add: {ex. self}.
		ex pass ].
	self hasBeenExecuted: correctlyExecuted.
]

{ #category : #accessing }
StartupAction >> hasBeenExecuted [

	^ hasBeenExecuted ifNil: [ hasBeenExecuted := false ]
]

{ #category : #accessing }
StartupAction >> hasBeenExecuted: anObject [
	
	hasBeenExecuted := anObject
]

{ #category : #accessing }
StartupAction >> name [

	^ name
]

{ #category : #accessing }
StartupAction >> name: anObject [
	
	name := anObject
]

{ #category : #printing }
StartupAction >> printOn: aStream [

	aStream <<'a ' << self class name << '(' << self name << ')'
]

{ #category : #accessing }
StartupAction >> runOnce [

	^ runOnce ifNil: [ runOnce := false ]
]

{ #category : #accessing }
StartupAction >> runOnce: anObject [
	
	runOnce := anObject
]

{ #category : #printing }
StartupAction >> storeOn: aStream [

	aStream << self class name << ' name: '<< self name printString <<' code: ' << code printString.
	self runOnce ifTrue: [ aStream << ' runOnce: true']
]
