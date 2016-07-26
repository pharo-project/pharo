private
evaluatePragma: pragma
	currentContext := pragma.
	[ self configuration perform: pragma methodSelector with: self ]
		ensure: [ currentContext := nil ]