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
SendsDeprecatedMethodToGlobalRule >> check: aNode forCritiquesDo: aCriticBlock [
	| messageReceiver |

	aNode isMessage ifFalse: [ ^ self ].
	
	messageReceiver := aNode receiver.
	messageReceiver isVariable ifFalse: [ ^ self ].
	messageReceiver isGlobal ifFalse: [ ^ self ].
	(self
		check: aNode selector
		forDeprecationIn: messageReceiver name) ifFalse: [ ^ self ].
		
	aCriticBlock cull: (self critiqueFor: aNode)
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
