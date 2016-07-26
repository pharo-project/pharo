accessing
forcedMethod
	"Return a symbol describing an enforced method or nil if the method should
	be chosen adaptively. Valid symbols are
		#stored	- store blocks (do not compress)
		#fixed	- use fixed huffman trees
		#dynamic	- use dynamic huffman trees."
	^nil