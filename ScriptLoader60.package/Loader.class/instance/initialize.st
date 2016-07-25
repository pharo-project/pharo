initialization
initialize
	super initialize.
	PackagesBeforeLastLoad ifNil: [ PackagesBeforeLastLoad := Set new ]