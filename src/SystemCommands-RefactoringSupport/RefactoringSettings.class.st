Class {
	#name : #RefactoringSettings,
	#superclass : #Object,
	#classVars : [
		'ScopeList'
	],
	#category : #'SystemCommands-RefactoringSupport-Scope'
}

{ #category : #adding }
RefactoringSettings class >> addScope: aScope [ 
	self scopeList add: aScope
]

{ #category : #adding }
RefactoringSettings class >> addScope: aScope withKey: key [
	aScope label: key.
	self scopeList add: aScope
]

{ #category : #accesing }
RefactoringSettings class >> availableScopes [
	"We don't return the Current image scope"
	^ self scopeList allButFirst
]

{ #category : #accesing }
RefactoringSettings class >> invalidNames [ 
	^ self scopeList collect: [ :e | e label ]
]

{ #category : #adding }
RefactoringSettings class >> removeScopeNamed: key [ 
	self scopeList removeAllSuchThat: 
		[ :e | e description = key and: 
			[ e description ~= 'Current image'] ]
]

{ #category : #accesing }
RefactoringSettings class >> scopeList [

	^ ScopeList ifNil: 
		[ ScopeList := OrderedCollection with: 
			(ClyNavigationEnvironment currentImageScope asRBEnvironment label: 'current image') ]
]

{ #category : #accesing }
RefactoringSettings class >> scopeList: anObject [

	ScopeList := anObject
]
