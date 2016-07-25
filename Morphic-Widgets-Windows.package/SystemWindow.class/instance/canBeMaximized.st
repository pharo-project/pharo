testing
canBeMaximized
	"Answer whether we are not we can be maximised."

	^ self isResizeable and: [ self isNotMaximized ]