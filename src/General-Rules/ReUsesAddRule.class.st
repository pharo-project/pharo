"
Check for possible uses of the result returned by an add: or addAll: messages. These messages return their arguments not the receiver. As a result, may uses of the results are wrong.
"
Class {
	#name : 'ReUsesAddRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Potential Bugs',
	#package : 'General-Rules',
	#tag : 'Potential Bugs'
}

{ #category : 'accessing' }
ReUsesAddRule class >> group [
	^ 'Potential Bugs'
]

{ #category : 'accessing' }
ReUsesAddRule class >> ruleName [
	^ 'Uses the result of an add: message'
]

{ #category : 'accessing' }
ReUsesAddRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'UsesAddRule'
]

{ #category : 'running' }
ReUsesAddRule >> basicCheck: node [
	^ node isMessage and: [ (#(#add: #addAll:) includes: node selector) and: [ node isEssential ] ]
]
