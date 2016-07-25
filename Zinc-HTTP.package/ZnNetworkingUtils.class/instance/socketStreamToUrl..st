public
socketStreamToUrl: url
	url hasHost ifFalse: [ ZnMissingHost signal ].  
	^ (self shouldProxyUrl: url)
		ifTrue: [ self socketStreamToProxy ] 
		ifFalse: [ self socketStreamToUrlDirectly: url ]