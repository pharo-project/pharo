tests
testHeadReference
	1 to: 7 do: [ :each |
		store newEntry: (OmEntry content: each) ].
	
	self 
		assert: store headReference
		equals: (store referenceTo: (store entries detect: [ :entry | entry content = 7 ]))