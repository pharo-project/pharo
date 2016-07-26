operations
linkToAuthRequest: aHeader
	"Link this credential to the WWW-Authenticate request in the provided header. 
	Used for authentication methods which require parameters provided by the server
	(e.g. Digest authentication)."
	realm := self class findRealmFor: aHeader