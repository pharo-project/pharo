statusbar
addDefaultStatusbarTransmissionFrom: aPane
	| defaultTransmission |
	defaultTransmission := GLMTransmission new
		ensureReplacePresentationsStrategy;
		addActiveOrigin: (aPane port: #status); 
		destination: (self statusbarPane port: #entity); 
		addPresentation: self statusbarPresentation; 
		yourself.
	self addTransmission: defaultTransmission.