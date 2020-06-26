"
Check for blocks that are immediately evaluated. Since the block is immediately evaluated, there is no need for the statements to be in a block.
For example, [:x | 1 + x] value: 4 is equivalent to 1 + 4
"
Class {
	#name : #ReExtraBlockRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReExtraBlockRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ExtraBlockRule'
]

{ #category : #enumerating }
ReExtraBlockRule >> basicCheck: node [
	node isMessage ifFalse: [ ^ false ].
	node receiver isBlock ifFalse: [ ^ false ].
	node parent isCascade ifTrue: [ ^ false ].
	^ self blockEvaluatingMessages includes: node selector
]

{ #category : #enumerating }
ReExtraBlockRule >> blockEvaluatingMessages [
	^ #(value 
	value: value:value: value:value:value: value:value:value:value:
	cull: cull:cull: cull:cull:cull: cull:cull:cull:cull:
	#valueWithArguments: valueWithPossibleArgs: valueWithPossibleArgument)
]

{ #category : #accessing }
ReExtraBlockRule >> group [
	^ 'Optimization'
]

{ #category : #accessing }
ReExtraBlockRule >> name [
	^ 'Block immediately evaluated'
]

{ #category : #accessing }
ReExtraBlockRule >> severity [
	^ #information
]
