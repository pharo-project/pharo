private
openConnectionToHost: host port: port
	^ self socketStreamClass 
		openConnectionToHost: host port: port