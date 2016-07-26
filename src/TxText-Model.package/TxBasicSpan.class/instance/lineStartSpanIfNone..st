accessing
lineStartSpanIfNone: aBlock
	self reverseDo: [ :span | 
		span isAtLineStart ifTrue: [ ^ span ]].
	^ aBlock value 