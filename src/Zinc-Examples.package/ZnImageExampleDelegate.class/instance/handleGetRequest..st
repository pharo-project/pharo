request handling
handleGetRequest: request
	"GET /image - Serve an HTML page showing our image and a form to upload a new one.
	GET /image?raw=true - Serve the last image directly"
	
	^ (request uri queryAt: #raw ifAbsent: [ nil ])
		ifNil: [ 
			ZnResponse ok: (ZnEntity html: self html) ]
		ifNotNil: [ 
			ZnResponse ok: self image ]