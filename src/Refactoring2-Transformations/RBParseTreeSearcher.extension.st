Extension { #name : #RBParseTreeSearcher }

{ #category : #'*Refactoring2-Transformations' }
RBParseTreeSearcher class >> matchingTreeForMethod: aCompiledMethod [

	| searchCode tree variables |
	tree := aCompiledMethod ast.
	searchCode := tree body sourceCode.
	
	"if there's just a return, remove it"
	tree body lastIsReturn ifTrue: [ 
		searchCode := searchCode copyReplaceAll: '^' with: '' ].
	
	"replace arguments by wildcards"
	variables := tree arguments
		collect: [ :e | ' ', e name asString ].
	variables doWithIndex: [ :e :i |
		searchCode := searchCode copyReplaceAll: e with: '``@a',i asString ].
	
	^ searchCode
]

{ #category : #'*Refactoring2-Transformations' }
RBParseTreeSearcher class >> matchingTreeForSubtree: subtree [

	| searchCode argumentNodes |
	searchCode := subtree formattedCode.

	"if there's just a return, remove it"
	subtree lastIsReturn ifTrue: [ 
		searchCode := searchCode copyReplaceAll: '^' with: '' ].
	
	"replace literal values by wildcards"
	argumentNodes := subtree allChildren
		select: #isLiteralNode
		thenCollect: [ :e | ' ', e value printString ].
	argumentNodes doWithIndex: [ :e :i |
		searchCode := searchCode
							copyReplaceAll: e 
							with: '``@a',i asString,'`{ :aNode | aNode isArgument }' ].
	
	^ searchCode
]

{ #category : #'*Refactoring2-Transformations' }
RBParseTreeSearcher class >> treeMatchings: aString in: aParseTree [ 

	| allMatches |
	allMatches := OrderedCollection new.
	self new
		matches: aString do: [ :aNode :answer | allMatches add: aNode ];
		executeTree: aParseTree.
	^ allMatches
]

{ #category : #'*Refactoring2-Transformations' }
RBParseTreeSearcher class >> whichMethodIn: aCollection matches: subtree [

	| isGetterBlock searchTree |
	isGetterBlock := self new
		matchesAnyMethodOf: #(
			'`method ^ `@aValue`{ :aNode |
				(aNode isKindOf: RBInstanceVariableNode)
				or: [ aNode isKindOf: RBLiteralValueNode ] }' )
		do: [ :node :answer | true ].
	searchTree := self matchingTreeForSubtree: subtree.

	^ aCollection
		reject: [ :method | (isGetterBlock executeTree: method ast initialAnswer: false)
			or: [ (self treeMatching: searchTree in: method ast) isNil ] ]
]
