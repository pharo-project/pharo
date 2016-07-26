ui process
spawnNewProcessIfThisIsUI: suspendedProcess

	self uiProcess == suspendedProcess ifTrue: [
		self spawnNewProcess.
		^true
	].
	^false		"no new process was created"
