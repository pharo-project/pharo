event testing
handlesMouseWheel: event
	^self isVerticalScrollBarVisible and: [ self hasDataSource ]