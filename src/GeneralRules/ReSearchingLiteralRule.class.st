"
Checks for repeated literal equality tests that should rather be implemented as a search in a literal collection.
"
Class {
	#name : #ReSearchingLiteralRule,
	#superclass : #ReNodeMatchRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReSearchingLiteralRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'SearchingLiteralRule'
]

{ #category : #hooks }
ReSearchingLiteralRule >> afterCheck: aNode mappings: mappingDict [
	^ self
		isSearchingLiteralExpression: aNode
		for: (mappingDict at: '``@object')
]

{ #category : #accessing }
ReSearchingLiteralRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
ReSearchingLiteralRule >> initialize [
	super initialize.
	self matchesAny: #(
			'``@object = `#literal or: [``@expression]' 
			'``@object == `#literal or: [``@expression]' 
			'`#literal = ``@object or: [``@expression]' 
			'`#literal == ``@object or: [``@expression]' 
			'``@expression | (``@object = `#literal)' 
			'``@expression | (``@object == `#literal)' 
			'``@expression | (`#literal = ``@object)' 
			'``@expression | (`#literal == ``@object)')
]

{ #category : #private }
ReSearchingLiteralRule >> isSearchingLiteralExpression: aSearchingNode for: anObjectNode [ 
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
ReSearchingLiteralRule >> name [
	^ 'Uses or instead of a searching literal'
]
