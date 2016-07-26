accessing
closureCreationNode
	closureCreationNode ifNil:
		[closureCreationNode := LeafNode new
									key: #closureCreationNode
									code: nil].
	^closureCreationNode