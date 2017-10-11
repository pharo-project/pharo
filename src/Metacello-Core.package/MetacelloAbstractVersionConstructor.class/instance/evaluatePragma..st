private
evaluatePragma: pragma
	currentContext := pragma.
	[ self configuration 
		perform: (MetacelloPlatform current selectorForPragma: pragma) 
		with: self ] 
	ensure: [ currentContext := nil ]