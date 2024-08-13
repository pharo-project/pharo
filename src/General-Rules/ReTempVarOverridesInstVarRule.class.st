"
Finds methods whose arguments or temporary variables ooverride an instance variable. This causes problems if you want to use the instance variable inside the method.
"
Class {
	#name : 'ReTempVarOverridesInstVarRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Potential Bugs',
	#package : 'General-Rules',
	#tag : 'Potential Bugs'
}

{ #category : 'testing-interest' }
ReTempVarOverridesInstVarRule class >> checksMethod [
	^ true
]

{ #category : 'accessing' }
ReTempVarOverridesInstVarRule class >> group [
	^ 'Potential Bugs'
]

{ #category : 'accessing' }
ReTempVarOverridesInstVarRule class >> ruleName [
	^ 'Outer variable shadowed by temporary variable or argument'
]

{ #category : 'accessing' }
ReTempVarOverridesInstVarRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'TempVarOverridesInstVarRule'
]

{ #category : 'running' }
ReTempVarOverridesInstVarRule >> check: aMethod forCritiquesDo: aCriticBlock [

	| problemTemps |
	problemTemps := aMethod ast variableDefinitionNodes
		 collect: [ :node | node variable ]
		 thenSelect: [ :var | var isShadowing ].
	problemTemps do: [ :var |
		aCriticBlock cull:
			(self critiqueFor: aMethod about: var definingNode) ]
]

{ #category : 'running - helpers' }
ReTempVarOverridesInstVarRule >> critiqueFor: aMethod about: aVarNode [
	^ (ReTrivialCritique
		withAnchor: (ReIntervalSourceAnchor
			entity: aMethod
			interval: aVarNode sourceInterval)
		by: self)
		tinyHint: aVarNode name;
		yourself
]
