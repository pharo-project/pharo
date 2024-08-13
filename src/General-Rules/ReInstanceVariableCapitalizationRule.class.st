"
Instance variable names on the instance- and class-side should start with a lowercase letter.
"
Class {
	#name : 'ReInstanceVariableCapitalizationRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Style',
	#package : 'General-Rules',
	#tag : 'Style'
}

{ #category : 'testing - interest' }
ReInstanceVariableCapitalizationRule class >> checksClass [
	^ true
]

{ #category : 'accessing' }
ReInstanceVariableCapitalizationRule class >> group [
	^ 'Style'
]

{ #category : 'accessing' }
ReInstanceVariableCapitalizationRule class >> ruleName [
	^ 'Instance variable capitalized'
]

{ #category : 'running' }
ReInstanceVariableCapitalizationRule >> check: aClass forCritiquesDo: aCriticBlock [

	aClass instVarNames
		select: [ :each | each first isUppercase ]
		thenDo: [ :each | aCriticBlock cull: (self critiqueFor: aClass about: each) ]
]

{ #category : 'running - helpers' }
ReInstanceVariableCapitalizationRule >> critiqueFor: aClass about: aVarName [

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
