private
openConnectionToHostNamed: host port: port
	^ self socketStreamClass 
		openConnectionToHostNamed: host port: port