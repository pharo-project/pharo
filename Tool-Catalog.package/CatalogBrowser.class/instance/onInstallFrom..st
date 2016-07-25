private - events
onInstallFrom: presenter

	self installStableVersion: presenter selection
		  onSuccess: [ presenter update ]