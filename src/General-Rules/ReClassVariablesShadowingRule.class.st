"
This rule checks if a instance or class variable shadows a global
"
Class {
	#name : 'ReClassVariablesShadowingRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Design Flaws',
	#package : 'General-Rules',
	#tag : 'Design Flaws'
}

{ #category : 'testing-interest' }
ReClassVariablesShadowingRule class >> checksClass [

	^ true
]

{ #category : 'accessing' }
ReClassVariablesShadowingRule class >> group [
	^ 'Design Flaws'
]

{ #category : 'accessing' }
ReClassVariablesShadowingRule class >> ruleName [
	^ 'Variable shadows a global variable'
]

{ #category : 'running' }
ReClassVariablesShadowingRule >> check: aClass forCritiquesDo: aCriticBlock [

	aClass definedVariables
		select: [ :variable | variable isShadowing ]
		thenDo: [ :variable | aCriticBlock cull: (self critiqueFor: aClass about: variable name) ]
]
