comparing
isAncestorOf: anotherCommit
	"Warning: this method considers that a commit is ancestor of itself."
	^ self = anotherCommit or: [ 
		"I can not be ancestor of a commit older than me so avoid traversing the graph"
		(self datetime <= anotherCommit datetime) and: [ 
			anotherCommit parents anySatisfy: [ :parent | 
				self isAncestorOf: parent  ]]]