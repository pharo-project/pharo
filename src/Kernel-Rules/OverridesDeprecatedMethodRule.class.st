"
The method overrided a deprecated method. This is a sign that an API has changed in an upstream project and most likely the method should override another one
"
Class {
	#name : #OverridesDeprecatedMethodRule,
	#superclass : #RBBlockLintRule,
	#category : #Kernel-Rules
}

{ #category : #testing }
OverridesDeprecatedMethodRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
OverridesDeprecatedMethodRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^ 'RBOverridesDeprecatedMethodRule'
]

{ #category : #running }
OverridesDeprecatedMethodRule >> basicCheck: aMethod [
	^ aMethod overriddenMethods anySatisfy: #isDeprecated
]

{ #category : #running }
OverridesDeprecatedMethodRule >> checkMethod: aMethod [

	(self basicCheck: aMethod) ifTrue: [ 
			result addMethod: aMethod ]
]

{ #category : #accessing }
OverridesDeprecatedMethodRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
OverridesDeprecatedMethodRule >> name [
	^ 'Overrides a deprecated method'
]

{ #category : #accessing }
OverridesDeprecatedMethodRule >> rationale [
	^ 'The method overrided a deprecated method. This is a sign that an API has changed in an upstream project and most likely the method should override another one'
]
