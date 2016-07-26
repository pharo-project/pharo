accessing
server
	"Returns the update server name"
	
	pharoServer ifNil: [ pharoServer := self defaultServer ].
	^ pharoServer