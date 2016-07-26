running
run

	[ self prepareForRunning.
	  CurrentJob value: self during: [ ^ block cull: self ] ]
			ensure: [ self cleanupAfterRunning ]