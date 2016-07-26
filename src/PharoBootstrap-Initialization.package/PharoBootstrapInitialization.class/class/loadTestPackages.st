loading
loadTestPackages
	self testPackages do: [ :each |
		self 
			loadPackage: each
			from: self directory ]