private
extractCommentsFrom: aToken
 
	aToken comments 
		collect: [:interval |
			"RBComment from: interval first to: interval last"
 			RBComment with: (source copyFrom: (interval first + 1) to: (interval last - 1)) at: interval first]
		into: comments