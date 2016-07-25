accessing
parseTree
	^RBParser parseMethod: self source onError: [:str :pos | ^nil]