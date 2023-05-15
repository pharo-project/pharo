Class {
	#name : #RefactoringSettings,
	#superclass : #Object,
	#classVars : [
		'ScopeList',
		'ScopeRefactoring'
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

{ #category : #accessing }
RefactoringSettings class >> availableScopes [
	"We don't return the Current image scope"
	^ self scopeList allButFirst
]

{ #category : #accessing }
RefactoringSettings class >> domainValues [

	^ self scopeList collect: [ :each |
		each description asSymbol -> each
	]
]

{ #category : #accessing }
RefactoringSettings class >> invalidNames [
	^ self scopeList collect: [ :e | e label ]
]

{ #category : #accessing }
RefactoringSettings class >> refactoringSettingsOn: aBuilder [

	<systemsettings>
	(aBuilder group: #refactoring)
		label: 'Refactoring Engine';
		description:
			'All refactoring related settings';
		noOrdering;
		with: [
			(aBuilder pickOne: #scopeRefactoring)
				order: 0;
				target: self;
				description: 'Selection new default scope for refactorings';
				label: 'Default scope';
				default: self scopeList first;
				domainValues: self domainValues.
			(aBuilder setting: #undoSize)
				target: RBRefactoryChangeManager;
				default: 20;
				label: 'Undo size';
				parentName: #refactoring ]
]

{ #category : #removing }
RefactoringSettings class >> removeScopeNamed: key [

	self scopeList removeAllSuchThat:
		[ :e | e description = key and:
			[ e description ~= 'Current image'] ]
]

{ #category : #accessing }
RefactoringSettings class >> scopeList [

	^ ScopeList ifNil:
		[ ScopeList := OrderedCollection with:
			(ClyNavigationEnvironment currentImageScope asRBEnvironment label: 'current image') ]
]

{ #category : #accessing }
RefactoringSettings class >> scopeList: anObject [

	ScopeList := anObject
]

{ #category : #accessing }
RefactoringSettings class >> scopeRefactoring [

	^ ScopeRefactoring ifNil: [ ScopeRefactoring := self scopeList first ]
]

{ #category : #accessing }
RefactoringSettings class >> scopeRefactoring: anObject [

	ScopeRefactoring := anObject
]

{ #category : #setting }
RefactoringSettings class >> setDescriptionOf: anItem with: aString [
	anItem label: aString
]
