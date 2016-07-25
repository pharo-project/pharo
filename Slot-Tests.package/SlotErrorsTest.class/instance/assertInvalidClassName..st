helpers
assertInvalidClassName: aName

	self should: [
		self make: [ :builder | 
			builder name: aName ] ]
		raise: InvalidGlobalName.
	self flag: 'should remove class if not raised'.
		
	self should: [
		PharoClassInstaller 
			validateClassName: aName ]
		raise: InvalidGlobalName