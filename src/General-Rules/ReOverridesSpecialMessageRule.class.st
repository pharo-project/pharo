"
Checks that a class does not override a message that is essential to the base system. For example, if you override the #class method from object, you are likely to crash your image. #classShouldNotOverride returns the list of messages which should not be overriden.
"
Class {
	#name : 'ReOverridesSpecialMessageRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Bugs',
	#package : 'General-Rules',
	#tag : 'Bugs'
}

{ #category : 'testing' }
ReOverridesSpecialMessageRule class >> checksClass [
	^ true
]

{ #category : 'private' }
ReOverridesSpecialMessageRule class >> classShouldNotOverride [
	^ #( #== #~~ #class #basicAt: #basicAt:put: #basicSize #identityHash )
]

{ #category : 'accessing' }
ReOverridesSpecialMessageRule class >> group [
	^ self bugsGroup
]

{ #category : 'private' }
ReOverridesSpecialMessageRule class >> metaclassShouldNotOverride [
	^ #( #basicNew #basicNew #class #comment #name #preferredExtent )
]

{ #category : 'accessing' }
ReOverridesSpecialMessageRule class >> rationale [

	^ 'Checks that a class does not override a message that is essential to the base system. For example, if you override the #class method from object, you are likely to crash your image.
In the class the messages we should not override are: ' , (', ' join: self classShouldNotOverride) , '.
In the class side the messages we should not override are: ' , (', ' join: self metaclassShouldNotOverride) , '.'
]

{ #category : 'accessing' }
ReOverridesSpecialMessageRule class >> ruleName [
	^ 'Overrides a "special" message'
]

{ #category : 'accessing' }
ReOverridesSpecialMessageRule class >> severity [
	^ #error
]

{ #category : 'accessing' }
ReOverridesSpecialMessageRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'OverridesSpecialMessageRule'
]

{ #category : 'running' }
ReOverridesSpecialMessageRule >> check: aClass forCritiquesDo: aCriticBlock [
	| selectors |
	selectors := aClass isMeta
		ifTrue: [ self metaclassShouldNotOverride ]
		ifFalse: [ self classShouldNotOverride ].
	(selectors anySatisfy: [ :each |
		(aClass superclass isNotNil
			and: [ (aClass superclass canUnderstand: each)
			and: [ (aClass includesSelector: each) ] ]) ]) ifTrue: [
		aCriticBlock cull: (self critiqueFor: aClass) ]
]

{ #category : 'private' }
ReOverridesSpecialMessageRule >> classShouldNotOverride [
	^ self class classShouldNotOverride
]

{ #category : 'private' }
ReOverridesSpecialMessageRule >> metaclassShouldNotOverride [
	^ self class metaclassShouldNotOverride
]
