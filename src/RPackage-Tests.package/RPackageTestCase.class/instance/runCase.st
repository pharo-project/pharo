running
runCase
	[
	self initializeAnnouncers.
	
	^ self packageClass 
		withOrganizer: self setupOrganizer
		do: [ 
			self resources do: [:each | each availableFor: self].
			self setUp.
			self performTest
			]
	] ensure: [ 
		self tearDown.
		self restoreAnnouncers.
		self cleanUpInstanceVariables
	]