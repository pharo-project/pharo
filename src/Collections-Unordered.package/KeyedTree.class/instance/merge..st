adding
merge: aKeyedTree
	"Merge the given tree into the receiver, overwriting or extending elements as needed."

	aKeyedTree keysAndValuesDo: [ :k :v | 
			| subtree |
			(v isKindOf: KeyedTree)
				ifTrue: [ 
					subtree := self at: k ifAbsentPut: [ v species new ].
					(subtree isKindOf: KeyedTree)
						ifFalse: [ subtree := self at: k put: v species new ].
					subtree merge: v ]
				ifFalse: [ self at: k put: v ] ]