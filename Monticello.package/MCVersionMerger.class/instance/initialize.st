initialization
initialize
	super initialize.
	records := OrderedCollection new.
	merger := MCThreeWayMerger new.