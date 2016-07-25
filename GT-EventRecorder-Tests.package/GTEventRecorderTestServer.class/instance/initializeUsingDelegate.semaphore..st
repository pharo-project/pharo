initialization
initializeUsingDelegate: aDelegate semaphore: aSemaphore
	| subscription |
	server := ZnServer on: 0.
	server delegate: aDelegate.
	server route: UUID new asString.
	subscription := ZnLogEvent announcer 
							when: ZnServerSocketBoundEvent 
							do: [ :announcement | announcement serverId = server route 
										ifTrue: [ aSemaphore signal ] ]
							for: self.
	server start.
	[ (aSemaphore waitTimeoutMSecs: 200) not ] assert.
	ZnLogEvent announcer removeSubscription: subscription.
	url := ZnUrl new
		scheme: #http;
		host: NetNameResolver loopBackName;
		port: server serverSocket port;
		yourself.