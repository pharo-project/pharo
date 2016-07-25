initialization
registerEvents
	super registerEvents.
	handlesDoubleClick whenChangedDo: [ :bool | self changed: { #handlesDoubleClick . bool } ].
