public
format: event on: stream
	"Output a ZnServerTransactionEvent on a text stream in Common Log Format"
	
	stream << (self clientIPAddressFrom: event); << ' - '.
	stream << (self userFrom: event); space.
	self formatTimestamp: event timestamp on: stream.
	self formatRequest: event on: stream.
	self formatResponse: event on: stream. 
	combined ifTrue: [ self formatCombinedFields: event request on: stream ]