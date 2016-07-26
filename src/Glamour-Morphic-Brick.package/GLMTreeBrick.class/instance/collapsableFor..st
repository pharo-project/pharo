as yet unclassified
collapsableFor: anObject
	|collapsable|
	
	collapsable := GLMCollapsableBrick new.
	collapsable
		vShrinkWrap;
		hSpaceFill.
	
	collapsable headerBrick: (self headerFor: anObject in: collapsable).
	(self hasChildren: anObject)
		ifTrue: [
			collapsable contentBrick: (self class new on: anObject value).
			collapsable contentBrick
				vShrinkWrap;
				hSpaceFill;
				marginLeft: 19 ].
	collapsable expand.
	^ collapsable