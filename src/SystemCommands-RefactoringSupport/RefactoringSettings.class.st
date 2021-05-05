Class {
	#name : #RefactoringSettings,
	#superclass : #Object,
	#classVars : [
		'ScopeList'
	],
	#category : #'SystemCommands-RefactoringSupport-Scope'
}

{ #category : #adding }
RefactoringSettings class >> addScope: aScope withKey: key [
	aScope label: key.
	self scopeList add: aScope
]

{ #category : #accesing }
RefactoringSettings class >> invalidNames [ 
	^ self scopeList collect: [ :e | e label ]
]

{ #category : #adding }
RefactoringSettings class >> removeScopeNamed: key [ 
	self scopeList removeAllSuchThat: [ :e | e label = key ]
]

{ #category : #accesing }
RefactoringSettings class >> scopeList [

	^ ScopeList ifNil: [ ScopeList := Dictionary new ]
]

{ #category : #accesing }
RefactoringSettings class >> scopeList: anObject [

	ScopeList := anObject
]
