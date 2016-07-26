visiting
visitCascadeNode: aCascadeNode
	aCascadeNode receiver accept: self.
	aCascadeNode messages do:
		[:message| self visitMessageNodeInCascade: message]