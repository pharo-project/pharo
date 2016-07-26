private-events
classRenamed: anAnnouncement
	" TODO: example dependencies and/or proxies might be invalid !!! (string/symbol-references) "
	self withoutExceptionsDo: [ 
		examples at: anAnnouncement oldName asSymbol ifPresent: [ :oldValue | 
			examples at: anAnnouncement newName asSymbol put: oldValue ].
		self checkForMissingDependencies ]