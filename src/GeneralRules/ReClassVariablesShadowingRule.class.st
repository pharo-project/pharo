"
This rule checks if a instance or class variable shadows a global
"
Class {
	#name : #ReClassVariablesShadowingRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #'testing-interest' }
ReClassVariablesShadowingRule class >> checksClass [

	^ true
]

{ #category : #running }
ReClassVariablesShadowingRule >> check: aClass forCritiquesDo: aCriticBlock [

	aClass definedVariables do: [ :variable | 
		variable isShadowing  ifTrue: [ 
			aCriticBlock cull: (self critiqueFor: aClass about: variable name) ] ]
]

{ #category : #'running-helpers' }
ReClassVariablesShadowingRule >> critiqueFor: aClass about: aVarName [

	| crit |
	crit := ReTrivialCritique
		withAnchor: (ReVarSearchSourceAnchor
			entity: aClass
			string: aVarName)
		by: self.
	
	crit tinyHint: aVarName.
				
	^ crit
]

{ #category : #accessing }
ReClassVariablesShadowingRule >> group [
	^ 'Design Flaws'
]

{ #category : #accessing }
ReClassVariablesShadowingRule >> name [
	^ 'Variable shadows a global variable'
]
