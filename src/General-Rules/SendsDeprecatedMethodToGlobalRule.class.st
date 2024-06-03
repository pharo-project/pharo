"
Checks for sends of deprecated messages that might be removed in upcoming releases of Pharo.
"
Class {
	#name : 'SendsDeprecatedMethodToGlobalRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Design Flaws',
	#package : 'General-Rules',
	#tag : 'Design Flaws'
}

{ #category : 'accessing' }
SendsDeprecatedMethodToGlobalRule class >> group [
	^ 'Design Flaws'
]

{ #category : 'accessing' }
SendsDeprecatedMethodToGlobalRule class >> ruleName [
	^ 'Sends a deprecated message to a known global'
]

{ #category : 'accessing' }
SendsDeprecatedMethodToGlobalRule class >> severity [
	^ #error
]

{ #category : 'accessing' }
SendsDeprecatedMethodToGlobalRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'SendsDeprecatedMethodToGlobalRule'
]

{ #category : 'running' }
SendsDeprecatedMethodToGlobalRule >> basicCheck: aNode [
	| value |
	aNode isMessage ifFalse: [ ^ false ].
	aNode receiver isLiteralVariable ifFalse: [ ^ false ].
	value := aNode receiver variable value.
	^value class classAndMethodFor: aNode selector
		do: [ :class :method | method isDeprecated ]
		ifAbsent: [ false ]
]
