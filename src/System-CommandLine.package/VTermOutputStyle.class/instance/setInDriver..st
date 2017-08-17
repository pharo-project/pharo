styling
setInDriver: aVTermOutputDriver
	font setInDriver: aVTermOutputDriver.
	background setInDriver: aVTermOutputDriver.
	self styles do: [ :each | aVTermOutputDriver set: each ].
	aVTermOutputDriver installedStyle: self.
	