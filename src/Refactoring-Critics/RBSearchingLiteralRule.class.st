"
Checks for repeated literal equalitity tests that should rather be implemented as a search in a literal collection.
"
Class {
	#name : #RBSearchingLiteralRule,
	#superclass : #RBParseTreeLintRule,
	#category : 'Refactoring-Critics-ParseTreeRules'
}

{ #category : #accessing }
RBSearchingLiteralRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'SearchingLiteralRule'
]

{ #category : #accessing }
RBSearchingLiteralRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
RBSearchingLiteralRule >> initialize [
	super initialize.
	self matcher
		matchesAnyOf: #(
			'``@object = `#literal or: [``@expression]' 
			'``@object == `#literal or: [``@expression]' 
			'`#literal = ``@object or: [``@expression]' 
			'`#literal == ``@object or: [``@expression]' 
			'``@expression | (``@object = `#literal)' 
			'``@expression | (``@object == `#literal)' 
			'``@expression | (`#literal = ``@object)' 
			'``@expression | (`#literal == ``@object)')
		do: [ :node :answer | 
			answer isNil 
				ifTrue: [
					(self isSearchingLiteralExpression: node)
						ifTrue: [ node ]
						ifFalse: [ nil ] ]
				ifFalse: [ answer ] ]
]

{ #category : #private }
RBSearchingLiteralRule >> isSearchingLiteralExpression: aMessageNode [ 
	| equalNode expressionNode |
	equalNode := aMessageNode selector = #| 
				ifTrue: [aMessageNode arguments first]
				ifFalse: [aMessageNode receiver].
	expressionNode := equalNode receiver isLiteralNode
				ifTrue: [equalNode arguments first]
				ifFalse: [equalNode receiver].
	^self isSearchingLiteralExpression: aMessageNode for: expressionNode
]

{ #category : #private }
RBSearchingLiteralRule >> isSearchingLiteralExpression: aSearchingNode for: anObjectNode [ 
	| argument arguments |
	aSearchingNode isMessage ifFalse: [^false].
	arguments := aSearchingNode arguments.
	arguments size = 1 ifFalse: [^false].
	argument := arguments first.
	(#(#= #==) includes: aSearchingNode selector) 
		ifTrue: 
			[^(aSearchingNode receiver = anObjectNode 
				and: [aSearchingNode arguments first isLiteralNode]) or: 
						[aSearchingNode arguments first = anObjectNode 
							and: [aSearchingNode receiver isLiteralNode]]].
	aSearchingNode selector = #| 
		ifTrue: 
			[^(self isSearchingLiteralExpression: aSearchingNode receiver
				for: anObjectNode) 
					and: [self isSearchingLiteralExpression: argument for: anObjectNode]].
	aSearchingNode selector = #or: ifFalse: [^false].
	argument isBlock ifFalse: [^false].
	argument body statements size = 1 ifFalse: [^false].
	^(self isSearchingLiteralExpression: aSearchingNode receiver
		for: anObjectNode) and: 
				[self isSearchingLiteralExpression: argument body statements first
					for: anObjectNode]
]

{ #category : #accessing }
RBSearchingLiteralRule >> name [
	^ 'Uses or''s instead of a searching literal'
]
