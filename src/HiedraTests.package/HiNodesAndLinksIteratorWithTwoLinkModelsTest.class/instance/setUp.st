running
setUp
	super setUp.
	
	parentsLinkBuilder := HiLinkBuilder new
		label: #parents;
		targetsBlock: [ :node | node parents ];
		yourself.
	originLinkBuilder := HiLinkBuilder new
		label: #origin;
		targetsBlock: [ :node | 
			node origin 
				ifNil: [ #() ]
				ifNotNil: [ { node origin } ] ];
		yourself.