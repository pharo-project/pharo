"
Finds methods whose temporary variables override an instance variable. This causes problems if you want to use the instance variable inside the method.
"
Class {
	#name : #RBTempVarOverridesInstVarRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #'testing-interest' }
RBTempVarOverridesInstVarRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBTempVarOverridesInstVarRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'TempVarOverridesInstVarRule'
]

{ #category : #running }
RBTempVarOverridesInstVarRule >> check: aMethod forCritiquesDo: aCriticBlock [
	| ivarNames |
	
	ivarNames := aMethod methodClass instVarNames.
	ivarNames ifEmpty: [ ^ self ].
	
	(aMethod ast arguments, aMethod ast temporaries)
		do: [ :node |
			(ivarNames includes: node name)
				ifTrue: [ 
					aCriticBlock cull: (
						self
							critiqueFor: aMethod
							about: node) ] ]
		

]

{ #category : #'running-helpers' }
RBTempVarOverridesInstVarRule >> critiqueFor: aMethod about: aVarNode [

	^ (ReTrivialCritique
		withAnchor: (ReIntervalSourceAnchor
			entity: aMethod
			interval: aVarNode sourceInterval)
		by: self)
		tinyHint: aVarNode name;
		yourself
]

{ #category : #accessing }
RBTempVarOverridesInstVarRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
RBTempVarOverridesInstVarRule >> name [
	^ 'Instance variable overridden by temporary variable'
]
