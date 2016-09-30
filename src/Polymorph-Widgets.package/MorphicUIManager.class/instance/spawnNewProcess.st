ui process
spawnNewProcess

	UIProcess := [
		[WorldMorph doOneCycle.  Processor yield.  false] whileFalse: [].
	] newProcess priority: Processor userSchedulingPriority.
	UIProcess name: 'Morphic UI Process'.
	UIProcess resume