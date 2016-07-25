accessing
extraHeaders
	^ extraHeaders ifNil: [ extraHeaders := ZnHeaders new ]