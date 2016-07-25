I am ZnValueDelegate.
I forward #handleRequest: messages to the object that I wrap using #value:

ZnServer default delegate: 
	(ZnValueDelegate with: [ :request | 
		ZnResponse ok: (ZnEntity with: 'You asked for ', request uri printString) ] ).

Part of Zinc HTTP Components.