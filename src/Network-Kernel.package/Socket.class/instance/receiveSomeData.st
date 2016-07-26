receiving
receiveSomeData
	"Receive currently available data (if any). Do not wait."
 
	| buffer bytesRead |
	buffer := String new: 2000.
	bytesRead := self receiveSomeDataInto: buffer.
	^buffer copyFrom: 1 to: bytesRead