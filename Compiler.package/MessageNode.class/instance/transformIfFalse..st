macro transformations
transformIfFalse: encoder
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				Array 
					with: ((BlockNode withJust: NodeNil) noteOptimizedIn: self)
					with: ((arguments at: 1) noteOptimizedIn: self).
			^true]
		ifFalse:
			[^false]