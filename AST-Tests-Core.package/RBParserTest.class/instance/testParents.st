tests
testParents
	(Array with: self treeWithEverything with: self treeWithReallyEverything) do: [ :tree |
		(Array with: tree with: tree copy) do: [ :root |
			root nodesDo: [ :node |
				node children do: [ :each |
					(each parent isMessage and: [ each parent isCascaded ]) ifFalse: [ 
						self assert: each parent == node.
						self assert: each methodNode == root ] ] ] ] ]