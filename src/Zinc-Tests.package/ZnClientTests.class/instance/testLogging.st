testing
testLogging
	| client logEventCollection count |
	logEventCollection := OrderedCollection new.
	ZnLogEvent announcer 
		when: ZnLogEvent do: [ :event | logEventCollection add: event ]. 
	(client := ZnClient new)
		clientId: #C1;
		beOneShot.
	client get: self smallHtmlUrl.
	client close.
	count := ZnLogEvent announcer numberOfSubscriptions.
	ZnLogEvent announcer 
		unsubscribe: self.
	self assert: ZnLogEvent announcer numberOfSubscriptions equals: count -1.
	self deny: logEventCollection isEmpty.
	self assert: logEventCollection anyOne clientId equals: #C1