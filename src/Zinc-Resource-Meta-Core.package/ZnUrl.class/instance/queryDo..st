accessing-query
queryDo: block
	"Execute block for each query key/value pair in the receiver"
	
	self hasQuery ifTrue: [ 
		self query keysAndValuesDo: block ]