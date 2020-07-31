"
List methods that have one or more arguments and do no refer to self or an instance variable. These methods might be better defined in some other class or as class methods.
"
Class {
	#name : #ReUtilityMethodsRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReUtilityMethodsRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
ReUtilityMethodsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UtilityMethodsRule'
]

{ #category : #running }
ReUtilityMethodsRule >> basicCheck: aMethod [
	aMethod methodClass isMeta ifTrue: [ ^false ].
	aMethod selector isUnary ifTrue: [ ^false ].
	(self utilityProtocols includes: aMethod protocol) ifTrue: [ ^false ].
	aMethod isOverridden ifTrue: [ ^ false ].
   aMethod sendsToSuper ifTrue: [ ^ false ].
	aMethod selfMessages isEmpty ifFalse: [ ^ false ].
	^aMethod methodClass definedVariables noneSatisfy: [ :variable | variable isAccessedIn: aMethod ]
]

{ #category : #accessing }
ReUtilityMethodsRule >> group [
	^ 'Design Flaws'
]

{ #category : #accessing }
ReUtilityMethodsRule >> name [
	^ 'Utility methods'
]

{ #category : #private }
ReUtilityMethodsRule >> utilityProtocols [
	"If a method is defined in one of these protocols, then don't check if its a utility method."
	^ #('utility' 'visiting' 'visitor')
]
