accessing
returnNilIfNoOther

	self returns
		ifFalse: 
			[statements last == NodeNil ifFalse: [statements add: NodeNil].
			self returnLast]