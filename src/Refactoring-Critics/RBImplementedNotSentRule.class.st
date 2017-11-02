"
This smell arises when a method is implemented but never sent. If a method is not sent, it can be removed. This rule pays attention not to identify as unsent methods, methods with pragmas and test methods since they are likely to be sent through reflection.
	Now if your code is used and extended by others better use a deprecation mechanism. To define a deprecate method follow the pattern: 
	
	foo
		self deprecated: ''Use bar instead ''. 
		^ self bar
		 
"
Class {
	#name : #RBImplementedNotSentRule,
	#superclass : #RBBlockLintRule,
	#instVars : [
		'literals'
	],
	#category : 'Refactoring-Critics-BlockRules'
}

{ #category : #testing }
RBImplementedNotSentRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBImplementedNotSentRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ImplementedNotSentRule'
]

{ #category : #private }
RBImplementedNotSentRule >> addLiteral: aLiteral [

	aLiteral isSymbol
		ifTrue: [ literals add: aLiteral ]
		ifFalse: [
			aLiteral class == Array ifTrue: [
				aLiteral do: [ :each |
					self addLiteral: each ] ] ]
]

{ #category : #running }
RBImplementedNotSentRule >> basicCheck: aMethod [
	"Check if there are any senders. Furthermore methods with pragmas are likely to be sent through reflection, thus do not report those. Also test methods are sent through reflection, so ignore those as well."
	(self literals includes: aMethod selector)
		ifTrue: [ ^ false ].
	aMethod pragmas isEmpty
		ifFalse: [ ^ false ].
	^ (aMethod methodClass isMeta not
		and: [ aMethod methodClass isTestCase ]) not
			
]

{ #category : #running }
RBImplementedNotSentRule >> checkMethod: aMethod [
	"Check if there are any senders. Furthermore methods with pragmas are likely to be sent through reflection, thus do not report those. Also test methods are sent through reflection, so ignore those as well."
	(self basicCheck: aMethod) ifTrue: [
		result addMethod: aMethod ]
]

{ #category : #accessing }
RBImplementedNotSentRule >> group [
	^ 'Design Flaws'
]

{ #category : #cache }
RBImplementedNotSentRule >> literals [
	"return all 'message sends' in the systed and cache them"

	literals ifNil: [
		literals := IdentitySet new.

		Smalltalk specialSelectors do: [:sel |
			sel isSymbol ifTrue: [ literals add: sel ] ].
		result rootEnvironment methodsDo: [ :meth |
			meth literals do: [ :lit | (meth hasLiteral: lit) ifTrue: [self addLiteral: lit]]]].
			"here we do a second check as #literals returns all literals even the one storing
			 method name"
	^ literals 
]

{ #category : #accessing }
RBImplementedNotSentRule >> name [
	^ 'Methods implemented but not sent'
]

{ #category : #accessing }
RBImplementedNotSentRule >> rationale [
	^ 'Check for methods that are never sent. If a method is not sent, it can be removed.'
]

{ #category : #running }
RBImplementedNotSentRule >> resetResult [
	super resetResult.
	literals := nil
]

{ #category : #accessing }
RBImplementedNotSentRule >> severity [
	^ #information
]
