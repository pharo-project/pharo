macro transformations
transformAnd: encoder
	(self transformBoolean: encoder)
		ifTrue: 
			[arguments := 
				Array 
					with: ((arguments at: 1) noteOptimizedIn: self)
					with: ((BlockNode withJust: NodeFalse) noteOptimizedIn: self).
			^true]
		ifFalse: 
			[^false]