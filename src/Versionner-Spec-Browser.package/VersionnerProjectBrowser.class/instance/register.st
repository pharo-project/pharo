announcement registration
register
	MBConfigurationRoot current announcer weak
		when: MBConfigurationInfoChanged
		send: #refreshConfigurationOfProject:
		to: self