macro transformations
transformWhile: encoder
	(self checkBlock: receiver as: 'receiver' from: encoder) ifFalse:
		[^false].
	arguments size = 0 ifTrue:  "transform bodyless form to body form"
		[selector := SelectorNode new
						key: (special = 10 ifTrue: [#whileTrue:] ifFalse: [#whileFalse:])
						code: #macro.
		 arguments := Array with: ((BlockNode withJust: NodeNil) noteOptimizedIn: self).
		 receiver noteOptimizedIn: self.
		 ^true].
	^(self transformBoolean: encoder)
	   and: [receiver noteOptimizedIn: self.
			arguments first noteOptimizedIn: self.
			true]