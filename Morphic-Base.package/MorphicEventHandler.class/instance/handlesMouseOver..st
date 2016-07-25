testing
handlesMouseOver: evt
	(self existsSubscriptionsFor: #mouseEnter) ifTrue: [ ^true ].
	(self existsSubscriptionsFor: #mouseLeave)  ifTrue: [ ^true ].
	(self existsSubscriptionsFor: #mouseOver) ifTrue: [ ^true ].
	^false.
		