extraction
extractMetadata

	^ {
		#author -> Author fullName.
		#version -> self extractVersion.
		#auto -> SlReporter autoSubmit
	} asDictionary