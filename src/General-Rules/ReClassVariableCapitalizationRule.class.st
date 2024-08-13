"
Class and pool variable names should start with an uppercase letter.
"
Class {
	#name : 'ReClassVariableCapitalizationRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Style',
	#package : 'General-Rules',
	#tag : 'Style'
}

{ #category : 'testing - interest' }
ReClassVariableCapitalizationRule class >> checksClass [
	^ true
]

{ #category : 'accessing' }
ReClassVariableCapitalizationRule class >> group [
	^ 'Style'
]

{ #category : 'accessing' }
ReClassVariableCapitalizationRule class >> ruleName [
	^ 'Class (or pool) variable not capitalized'
]

{ #category : 'running' }
ReClassVariableCapitalizationRule >> check: aClass forCritiquesDo: aCriticBlock [
	aClass isMeta ifTrue: [ ^ self ].
	aClass classVarNames, aClass sharedPoolNames
		select: [ :each | each first isLowercase ]
		thenDo: [ :each |
			aCriticBlock cull:
				(self critiqueFor: aClass about: each) ]
]

{ #category : 'running - helpers' }
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
			ReRenameSharedVariableRefactoring
				rename: aVarName
				to: aVarName capitalized
				in: aClass).

	^ crit
]
