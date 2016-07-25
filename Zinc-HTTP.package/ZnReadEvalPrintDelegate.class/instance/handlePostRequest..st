request handling
handlePostRequest: request
	^ ZnResponse ok: (ZnEntity text: (self evaluate: request entity contents))