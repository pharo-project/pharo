request handling
handleGetRequest: request
	^ ZnResponse ok: (ZnEntity text: (self helpText format: { request server port })) 