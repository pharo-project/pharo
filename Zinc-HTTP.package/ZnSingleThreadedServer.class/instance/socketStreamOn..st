private
socketStreamOn: socket
	self logConnectionAccepted: socket.
	^ ZnNetworkingUtils socketStreamOn: socket