what to view
actualViewee
	"Return the actual morph to be viewed, or nil if there isn't an appropriate morph to view."

	| aMorph actualViewee |
	aMorph := self morphToView ifNil: [^ nil]. 
	aMorph isInWorld ifFalse: [^ nil].
	actualViewee := viewSelector ifNil: [aMorph] ifNotNil: [objectToView perform: viewSelector].
	actualViewee = 0 ifTrue: [^ nil].  
	actualViewee ifNil: [actualViewee := objectToView].
	(actualViewee isMorph and: 
		[actualViewee isFlexMorph and: [actualViewee submorphs size = 1]])
			ifTrue: [actualViewee := actualViewee firstSubmorph].
	^ actualViewee