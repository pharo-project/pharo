protocol
saveGraphAndOpenWith: graphvizGenerator
	graphvizGenerator saveDotFileAndGenerateForType: self defaultFormat.
	^ graphvizGenerator openWith: self defaultFormat