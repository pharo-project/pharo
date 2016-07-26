ParseTreeSearcher walks over a normal source code parse tree using the visitor pattern, and then matches these nodes against the meta-nodes using the match:inContext: methods defined for the meta-nodes.

Instance Variables:
	answer	<Object>	the "answer" that is propagated between matches
	argumentSearches	<Collection of: (Association key: RBProgramNode value: BlockClosure)>	argument searches (search for the BRProgramNode and perform the BlockClosure when its found)
	context	<RBSmallDictionary>	a dictionary that contains what each meta-node matches against. This could be a normal Dictionary that is created for each search, but is created once and reused (efficiency).
	messages	<Collection>	the sent messages in our searches
	searches	<Collection of: (Association key: RBProgramNode value: BlockClosure)>	non-argument searches (search for the BRProgramNode and perform the BlockClosure when its found)