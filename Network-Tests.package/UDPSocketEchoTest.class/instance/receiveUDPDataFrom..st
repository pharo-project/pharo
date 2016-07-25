private
receiveUDPDataFrom: socket
	"Receive a UDP datagram from socket, return a 3-element array:
	{ contents. source host address. source port }
	We wait indefinitely for incoming data."
	
	| buffer result |
	buffer := String new: 256.
	socket waitForData.
	result := socket receiveUDPDataInto: buffer.
	^ { buffer copyFrom: 1 to: result first. result second. result third }