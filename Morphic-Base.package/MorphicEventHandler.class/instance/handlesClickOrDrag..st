testing
handlesClickOrDrag: evt
	^(self existsSubscriptionsFor: #click)
		or: [ (self existsSubscriptionsFor: #doubleClick) 
			or: [(self existsSubscriptionsFor: #startDrag)]].