*FuelSystem-FileRegistry
fileReaderServicesForFile: fullName suffix: suffix
	<fileService>
	suffix = 'fuel'
		ifFalse: [ ^ #() ].
	^ { self serviceFuelMaterialize }