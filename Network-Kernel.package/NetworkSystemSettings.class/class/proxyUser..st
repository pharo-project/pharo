settings
proxyUser: aUser
	"Set the HTTP proxy user. Can be empty or nil to reset"
	
	ProxyUser := aUser ifNotNil: [ aUser base64Encoded ]

