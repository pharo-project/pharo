"
Instance variable names on the instance- and class-side should start with a lowercase letter.
"
Class {
	#name : #GRInstanceVariableCapitalizationRule,
	#superclass : #ReAbstractRule,
	#category : #GeneralRules
}

{ #category : #'testing-interest' }
GRInstanceVariableCapitalizationRule class >> checksClass [
	^ true
]

{ #category : #running }
GRInstanceVariableCapitalizationRule >> check: aClass forCritiquesDo: aCriticBlock [
	
	aClass instVarNames
		select: [ :each | each first isUppercase ]
		thenDo: [ :each |
			aCriticBlock cull: (
				self critiqueFor: aClass about: each) ]
]

{ #category : #'running-helpers' }
GRInstanceVariableCapitalizationRule >> critiqueFor: aClass about: aVarName [

	| crit |
	crit := ReRefactoringCritique
		withAnchor: (ReVarSearchSourceAnchor
			entity: aClass
			string: aVarName)
		by: self.
	
	crit	
		tinyHint: aVarName;
		refactoring: (
			RBRenameInstanceVariableRefactoring
				rename: aVarName
				to: aVarName uncapitalized
				in: aClass).
				
	^ crit
]

{ #category : #accessing }
GRInstanceVariableCapitalizationRule >> group [
	^ 'Style'
]

{ #category : #accessing }
GRInstanceVariableCapitalizationRule >> name [
	^ 'Instance variable capitalized'
]
