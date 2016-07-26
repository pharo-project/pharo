testing
testTimeout
	self withServerDo: [ : server |
		self 
			should: [ 
				ZnConnectionTimeout 
					value: 1 
					during: [ ZnEasy get: (server localUrl addPathSegment: 'echo'; queryAt: 'delay' put: '2'; yourself) ] ] 
			raise: ConnectionTimedOut ]
	