"
Settings for the refactoring system
"
Class {
	#name : #RefactoringSettings,
	#superclass : #Object,
	#classVars : [
		'ScopeRefactoring'
	],
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #accessing }
RefactoringSettings class >> domainValues [

	^ { (#'All packages' -> ClySystemEnvironmentScope).
		  (#'Owner package' -> ClyPackageScope).
		  (#'Class hierarchy' -> ClyCompositeScope).
		  (#'Class' -> ClyBothMetaLevelClassScope) }
]

{ #category : #settings }
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
				default: ClySystemEnvironmentScope;
				domainValues: self domainValues.
			(aBuilder setting: #undoSize)
				target: RBRefactoryChangeManager;
				default: 20;
				label: 'Undo size';
				parentName: #refactoring ]
]

{ #category : #accessing }
RefactoringSettings class >> scopeRefactoring [

	^ ScopeRefactoring ifNil: [ ScopeRefactoring := ClySystemEnvironmentScope ]
]

{ #category : #accessing }
RefactoringSettings class >> scopeRefactoring: anObject [

	ScopeRefactoring := anObject
]
