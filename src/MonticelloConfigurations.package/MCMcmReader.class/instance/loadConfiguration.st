accessing
loadConfiguration
	stream reset.
	configuration := MCConfiguration fromArray: (MCScanner scan: stream).
	configuration name: self configurationName.
