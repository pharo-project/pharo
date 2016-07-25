accessing
request: url
	"Setup the receiver to request url"
	
	(url isNil or: [ url hasHost not ]) ifTrue: [ ^ self ].  
	self at: 'Host' put: url authority.
	(ZnNetworkingUtils proxyAuthorizationHeaderValueToUrl: url)
		ifNotNil: [ :value | self at: 'Proxy-Authorization' put: value ].    
  