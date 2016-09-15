running
setUp
	linkBuilder := HiLinkBuilder new
		targetsBlock: [ :node | node parents ];
		yourself.