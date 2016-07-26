accessing
title

	^ title ifNil: [ title := self downloadTitle ]