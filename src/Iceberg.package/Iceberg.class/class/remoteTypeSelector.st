accessing
remoteTypeSelector
	"Should be #scpURL or #httpsURL"
	^ RemoteTypeSelector ifNil: [ RemoteTypeSelector := #scpUrl ]