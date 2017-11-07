"
The method overrided a deprecated method. This is a sign that an API has changed in an upstream project and most likely the method should override another one
"
Class {
	#name : #OverridesDeprecatedMethodRule,
	#superclass : #ReAbstractRule,
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
OverridesDeprecatedMethodRule >> check: aMethod forCritiquesDo: aCriticBlock [

	(aMethod overriddenMethods anySatisfy: #isDeprecated) ifTrue: [ 
		aCriticBlock cull: (self critiqueFor: aMethod) ]

]

{ #category : #accessing }
OverridesDeprecatedMethodRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
OverridesDeprecatedMethodRule >> name [
	^ 'Overrides a deprecated method'
]
