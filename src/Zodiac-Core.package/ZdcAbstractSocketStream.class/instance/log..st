private
log: object
	debug ifTrue: [
		Transcript cr; show: object value ]
	