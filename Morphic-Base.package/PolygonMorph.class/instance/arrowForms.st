private
arrowForms
	"ArrowForms are computed only upon demand"
	arrowForms
		ifNotNil: [^ arrowForms].
	arrowForms := Array new.
	self hasArrows
		ifFalse: [^ arrowForms].
	(arrows == #forward
			or: [arrows == #both])
		ifTrue: [arrowForms := arrowForms
						copyWith: (self computeArrowFormAt: vertices last from: self nextToLastPoint)].
	(arrows == #back
			or: [arrows == #both])
		ifTrue: [arrowForms := arrowForms
						copyWith: (self computeArrowFormAt: vertices first from: self nextToFirstPoint)].
	^ arrowForms