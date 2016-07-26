private-events
classRemoved: anAnnouncement
	" TODO: example dependencies and/or proxies might be invalid !!! (string/symbol-references) "
	self withoutExceptionsDo: [ 
		(examples removeKey: anAnnouncement classRemoved theNonMetaClass name ifAbsent: [ nil ]) ifNotNil: [ :dict | 
			dict do: [ :example |
				[ example detachFromSystem ] on: Error do: [ " ignore " ].
				"self example: example removed: anAnnouncement" ].
			self checkForMissingDependencies.
			self checkForMissingClasses ] ]