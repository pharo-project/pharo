"
Checks for sends of deprecated messages that might be removed in upcoming releases of Pharo.
"
Class {
	#name : #SendsDeprecatedMethodToGlobalRule,
	#superclass : #ReNodeBasedRule,
	#category : #'Kernel-Rules'
}

{ #category : #accessing }
SendsDeprecatedMethodToGlobalRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'SendsDeprecatedMethodToGlobalRule'
]

{ #category : #running }
SendsDeprecatedMethodToGlobalRule >> basicCheck: aNode [
	| messageReceiver |
	aNode isMessage ifFalse: [ ^ false ].

	messageReceiver := aNode receiver.
	messageReceiver isVariable ifFalse: [ ^ false ].
	messageReceiver isGlobal ifFalse: [ ^ false ].
	(self check: aNode selector forDeprecationIn: messageReceiver name) ifFalse: [ ^ false ].

	^ true
]

{ #category : #running }
SendsDeprecatedMethodToGlobalRule >> check: aSelector forDeprecationIn: aGlobalName [

	^ (Smalltalk globals at: aGlobalName ifAbsent: [ ^ false ]) class
		classAndMethodFor: aSelector
		do: [ :class :method | method isDeprecated ]
		ifAbsent: [ false ]
]

{ #category : #accessing }
SendsDeprecatedMethodToGlobalRule >> group [
	^ 'Design Flaws'
]

{ #category : #accessing }
SendsDeprecatedMethodToGlobalRule >> name [
	^ 'Sends a deprecated message to a known global'
]

{ #category : #accessing }
SendsDeprecatedMethodToGlobalRule >> severity [
	^ #error
]
