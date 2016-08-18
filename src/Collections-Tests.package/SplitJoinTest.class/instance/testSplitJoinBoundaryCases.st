running
testSplitJoinBoundaryCases
	"Empty splitter, joiner or sequence."
	self assert: (#() join: (#() split: #())) equals: #().
	self assert: ('' join: ('' split: '')) equals: ''.
	self assert: ('' join: #('')) equals: ''. 

	"Overlapping splitters, or at end of sequence"
	self assert: ('an' join: ('an' split: 'banana')) equals: 'banana'.
	self assert: ('na' join: ('na' split: 'banana')) equals: 'banana'.
	self assert: ('ana' join: ('ana' split: 'banana')) equals: 'banana'.
