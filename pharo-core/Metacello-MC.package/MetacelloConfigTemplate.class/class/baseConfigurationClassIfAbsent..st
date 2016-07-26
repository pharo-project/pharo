private
baseConfigurationClassIfAbsent: aBlock

	^Smalltalk
		at: #'ConfigurationOf'
		ifAbsent: [ 
			self ensureMetacelloBaseConfiguration.
			Smalltalk at: #'ConfigurationOf' ifAbsent: aBlock ].