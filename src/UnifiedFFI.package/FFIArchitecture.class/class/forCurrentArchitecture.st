instance creation
forCurrentArchitecture
	^ (self allSubclasses detect: #isActive) uniqueInstance