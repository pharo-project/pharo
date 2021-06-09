"
Class and pool variable names should start with an uppercase letter.
"
Class {
	#name : #ReClassVariableCapitalizationRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #'testing-interest' }
ReClassVariableCapitalizationRule class >> checksClass [
	^ true
]

{ #category : #running }
ReClassVariableCapitalizationRule >> check: aClass forCritiquesDo: aCriticBlock [
	aClass isMeta ifTrue: [ ^ self ].
	aClass classVarNames, aClass sharedPoolNames
		select: [ :each | each first isLowercase ]
		thenDo: [ :each |
			aCriticBlock cull:
				(self critiqueFor: aClass about: each) ]
]

{ #category : #'running - helpers' }
ReClassVariableCapitalizationRule >> critiqueFor: aClass about: aVarName [

	| crit |
	crit := ReRefactoringCritique
		withAnchor: (ReVarSearchSourceAnchor
			entity: aClass
			string: aVarName)
		by: self.
	
	crit	
		tinyHint: aVarName;
		refactoring: (
			RBRenameClassVariableRefactoring
				rename: aVarName
				to: aVarName capitalized
				in: aClass).
				
	^ crit
]

{ #category : #accessing }
ReClassVariableCapitalizationRule >> group [
	^ 'Style'
]

{ #category : #accessing }
ReClassVariableCapitalizationRule >> name [
	^ 'Class (or pool) variable not capitalized'
]
