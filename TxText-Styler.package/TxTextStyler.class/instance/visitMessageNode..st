visiting
visitMessageNode: aMessageNode
	| style link hand |
	style := #keyword.
	(Symbol findInterned: aMessageNode selector asString)
		ifNil: [ 
			style := (Symbol thatStartsCaseSensitive: aMessageNode selector asString skipping: nil) isNil
				ifTrue: [ #undefinedKeyword ]
				ifFalse: [ #incompleteKeyword ] ].
	aMessageNode keywordsPositions
		do: [ :token | self addStyle: style startPosition: token stopPosition: token + aMessageNode selector size - 1 ].
	link := TxActionAttribute new
		filter: [ :evt | evt commandKeyPressed and: [ evt isMouseDown ] ];
		value:
				[ :evt | 
					evt shiftPressed
						ifTrue: [ 
							SystemNavigation new
								browseSendersOf: aMessageNode selector
								name: 'Senders of ' , aMessageNode selector
								autoSelect: aMessageNode selector ]
						ifFalse: [ SystemNavigation new browseAllImplementorsOf: aMessageNode selector ] ] yourself.
	hand := TxMouseCursorAttribute with: Cursor webLink.
	aMessageNode keywordsPositions
		do: [ :token | 
			self addAttribute: link startPosition: token stopPosition: token + aMessageNode selector size - 1.
			self addAttribute: hand startPosition: token stopPosition: token + aMessageNode selector size - 1 ].
	(aMessageNode isCascaded not or: [ aMessageNode isFirstCascaded ])
		ifTrue: [ self visitNode: aMessageNode receiver ].
	aMessageNode arguments do: [ :each | self visitNode: each ]