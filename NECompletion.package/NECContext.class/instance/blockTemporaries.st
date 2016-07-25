private-temporaries
blockTemporaries
	| blocks range vars |
	blocks := OrderedCollection new.
	1 
		to: completionIndex - 1
		do: 
			[ :index | 
			range := ranges at: index.
			self 
				handleBlockStack: blocks
				with: range.
			range isBlockTemporary ifTrue: 
				[ vars := blocks last.
				vars add: range ] ].
	^ self convertBlocksToVariables: blocks