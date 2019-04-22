"
Class and pool variable names should start with an uppercase letter.
"
Class {
	#name : #GRClassVariableCapitalizationRule,
	#superclass : #ReAbstractRule,
	#category : #GeneralRules
}

{ #category : #'testing-interest' }
GRClassVariableCapitalizationRule class >> checksClass [
	^ true
]

{ #category : #running }
GRClassVariableCapitalizationRule >> check: aClass forCritiquesDo: aCriticBlock [
	aClass isMeta ifTrue: [ ^ self ].
	aClass classVarNames, aClass sharedPoolNames
		select: [ :each | each first isLowercase ]
		thenDo: [ :each |
			aCriticBlock cull:
				(self critiqueFor: aClass about: each) ]
]

{ #category : #'running-helpers' }
GRClassVariableCapitalizationRule >> critiqueFor: aClass about: aVarName [

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
GRClassVariableCapitalizationRule >> group [
	^ 'Style'
]

{ #category : #accessing }
GRClassVariableCapitalizationRule >> name [
	^ 'Class (or pool) variable not capitalized'
]
