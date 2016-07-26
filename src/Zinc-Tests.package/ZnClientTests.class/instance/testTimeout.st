testing
testTimeout
	self withServerDo: [ :server |
		self 
			should: [ 
				ZnClient new 
					timeout: 1; 
					url: server localUrl;
					addPath: 'echo';
					queryAt: 'delay' put: '2'; 
					get ]
			raise: ConnectionTimedOut ]