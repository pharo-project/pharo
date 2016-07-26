settings
proxyPassword: aPassword
	"Set the HTTP proxy password. Can be empty or nil to reset"
	
	ProxyPassword := aPassword ifNotNil: [ aPassword base64Encoded ]
