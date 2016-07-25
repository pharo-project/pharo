I am ZnDispatcherDelegate, doing a straight-forward dispatching to mapped URLs.

ZnDispatcherDelegate API:

(server := ZnServer startDefaultOn: 9090)
	delegate: (ZnDispatcherDelegate new 
		map: '/hello' to: [ :request :response | response entity: (ZnEntity html: '<h1>hello server</h1>') ];
		map: '/counter' to: [ :request :response | counterApplication handleRequest: request response: response ]).