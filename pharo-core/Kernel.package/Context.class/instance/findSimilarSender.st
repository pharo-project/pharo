query
findSimilarSender
	"Return the closest sender with the same method, return nil if none found"

	^ self sender findContextSuchThat: [ :context | 
		context method == method ]