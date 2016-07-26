updating screen
deferUpdatesWhile: aBlock
	deferUpdates := true.
	aBlock ensure: [ deferUpdates := false. ].
	self useDeferredUpdates.
	