submorphs-add/remove
delete
	predecessor ifNotNil: [predecessor setSuccessor: successor].
	successor ifNotNil: [successor setPredecessor: predecessor.
						successor recomposeChain].
	super delete