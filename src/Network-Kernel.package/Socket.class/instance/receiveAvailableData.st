receiving
receiveAvailableData
	"Receive all available data (if any). Do not wait."
 
	| buffer bytesRead |
	buffer := String new: 2000.
	bytesRead := self receiveAvailableDataInto: buffer.
	^buffer copyFrom: 1 to: bytesRead