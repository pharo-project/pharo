accessing
topHeaderHeight
	| h |
	self topHeader 
		ifNil: [^ 0] 
		ifNotNil: [:th | 
			h := th borderWidth * 2.
			self columns do: [:col | h := h max: col height].
			^ h]
		