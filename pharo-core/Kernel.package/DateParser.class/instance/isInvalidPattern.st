parsing - private
isInvalidPattern

	^ year isNil or: [ month isNil or: [ day isNil or: [ invalidPattern ]]]