"
Checks for remove:''s of elements inside of collection iteration methods such as do:. These can cause the do: method to break since it will walk of the end of the collection. The common fix for this problem is to copy the collection before iterating over it.

For example turning

aCol do: [ :each |  ... aCol remove:... ]

into 

aCol copy do: [ :each |  ... aCol remove:... ]
"
Class {
	#name : #RBModifiesCollectionRule,
	#superclass : #RBParseTreeLintRule,
	#category : #Refactoring-Critics-ParseTreeRules
}

{ #category : #accessing }
RBModifiesCollectionRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ModifiesCollectionRule'
]

{ #category : #accessing }
RBModifiesCollectionRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
RBModifiesCollectionRule >> initialize [
	super initialize.
	self matcher 
		matchesAnyOf: #(
			'`@object do: [:`each | | `@temps | ``@.Statements]' 
			'`@object collect: [:`each | | `@temps | ``@.Statements]' 
			'`@object select: [:`each | | `@temps | ``@.Statements]' 
			'`@object reject: [:`each | | `@temps | ``@.Statements]' 
			'`@object inject: `@value into: [:`sum :`each | | `@temps | ``@.Statements]')
		do: [ :node :answer | 
			answer isNil 
				ifTrue: [
					(self modifiesTree: node receiver in: node arguments last) 
						ifTrue: [ node ]
						ifFalse: [ nil ] ]
				ifFalse: [ answer ] ]
]

{ #category : #private }
RBModifiesCollectionRule >> modifiesTree: aCollectionTree in: aParseTree [ 
	| notifier args |
	notifier := RBParseTreeSearcher new.
	args := Array with: (RBPatternVariableNode named: '`@object').
	notifier 
		matchesAnyTreeOf: (#(add: addAll: remove: removeAll:) collect: 
					[:each | 
					RBMessageNode 
						receiver: aCollectionTree copy
						selector: each
						arguments: args])
		do: [:aNode :answer | true].
	^notifier executeTree: aParseTree initialAnswer: false
]

{ #category : #accessing }
RBModifiesCollectionRule >> name [
	^ 'Modifies collection while iterating over it'
]

{ #category : #accessing }
RBModifiesCollectionRule >> rationale [
	^ 'Checks for remove:''s of elements inside of collection iteration methods such as do:. '
]
