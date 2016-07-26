private
parseTree
	^ RBParser parseMethod: source onError: [ :str :pos | ^ nil ]