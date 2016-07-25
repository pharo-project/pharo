actions
addConfigurationInfoFor: aConfigurationClass
	(self configurationInfoFor: aConfigurationClass) notNil
		ifTrue: [ ^ self ].
	self configurations
		add:
			((MBConfigurationInfo configurationClass: aConfigurationClass)
				configurationRoot: self;
				yourself).
	self announcer announce: MBConfigurationInfoCreated