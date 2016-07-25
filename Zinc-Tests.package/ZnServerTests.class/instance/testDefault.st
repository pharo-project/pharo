testing
testDefault
	| port server initialDefaultServer wasRunning |
	wasRunning := ZnServer default 
		ifNil: [ false ] 
		ifNotNil: [ ZnServer default isRunning ].
	initialDefaultServer := ZnServer stopDefault.
	self assert: ZnServer default isNil.
	port := self port.
	server := ZnServer startDefaultOn: port.
	self assert: ZnServer default notNil.
	self assert: ZnServer default == server.
	self assert: ZnServer default port = port.
	self assert: ZnServer default isRunning.
	self assert: (ZnServer managedServers includes: server).
	ZnServer stopDefault.
	self assert: ZnServer default isNil.
	self deny: server isRunning.
	self deny: (ZnServer managedServers includes: server).
	server := ZnServer startDefaultOn: port.
	"Starting the default again is actually a restart"
	ZnServer startDefaultOn: port.
	self assert: ZnServer default == server.
	ZnServer stopDefault.
	ZnServer adoptAsDefault: initialDefaultServer.
	self assert: initialDefaultServer == ZnServer default.
	wasRunning ifTrue: [ ZnServer default start ]