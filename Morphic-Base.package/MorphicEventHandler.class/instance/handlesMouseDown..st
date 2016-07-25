testing
handlesMouseDown: evt
	^(self existsSubscriptionsFor: #mouseDown)
		or: [ (self existsSubscriptionsFor: #mouseStillDown)
			or: [ (self existsSubscriptionsFor: #mouseUp)
				or: [ (self handlesClickOrDrag: evt)
					or: [ self handlesGestureStart: evt]]]].