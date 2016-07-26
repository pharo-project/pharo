macro transformations
transformRepeat: encoder
	"answer true if this #repeat message can be optimized"
	
	^(self checkBlock: receiver as: 'receiver' from: encoder)
	   and: [receiver noteOptimizedIn: self.
			true]