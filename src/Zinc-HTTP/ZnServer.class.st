"
I am ZnServer, an abstract superclass of HTTP Servers.
I am a facade for controlling a default server implementation.

I delegate my public class protocol methods to #defaultServerClass.

  ZnServer startDefaultOn: 1701.
  ZnClient new get: 'http://localhost:1701'.

Subclasses can register with me to have start/stop sent to them on System startUp/shutDown.
The default server instance will be registered automatically when it is started.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnServer,
	#superclass : #Object,
	#instVars : [
		'options',
		'sessionManager'
	],
	#classVars : [
		'AlwaysRestart',
		'ManagedServers'
	],
	#category : #'Zinc-HTTP-Client-Server'
}

{ #category : #public }
ZnServer class >> adoptAsDefault: server [
	"Adopt server as the default instance that we manage.
	If there was a previous default, stop it.
	Delegate to the default server class."
	
	self defaultServerClass adoptAsDefault: server
]

{ #category : #accessing }
ZnServer class >> alwaysRestart [
	^ AlwaysRestart ifNil: [ AlwaysRestart := true ]
]

{ #category : #accessing }
ZnServer class >> alwaysRestart: boolean [
	"Set if managed servers should always restart on image save, as opposed to only when quiting."

	^ AlwaysRestart := boolean
]

{ #category : #public }
ZnServer class >> default [
	"Return the default instance that we manage.
	Delegate to the default server class."
	
	^ self defaultServerClass default
]

{ #category : #public }
ZnServer class >> defaultOn: portNumber [
	"Return the default instance on a given port,
	Keep a reference to it in a class instance variable.
	If there was no previous default instance, create a new one.
	If there was a previous default instance, reuse it:
	if it was running stop it, change the port if necessary.
	Delegate to the default server class."

	^ self defaultServerClass defaultOn: portNumber 
]

{ #category : #accessing }
ZnServer class >> defaultServerClass [
	"Return the default ZnServer subclass to use"
	
	^ ZnManagingMultiThreadedServer 
]

{ #category : #'class initialization' }
ZnServer class >> initialize [
	ManagedServers := IdentitySet new.
	AlwaysRestart := true.
	self environment 
		at: #SessionManager 
		ifPresent: [ :manager | manager default registerNetworkClassNamed: self name ]
		ifAbsent: [ Smalltalk addToStartUpList: self; addToShutDownList: self ]
	
]

{ #category : #accessing }
ZnServer class >> managedServers [
	^ ManagedServers ifNil: [ ManagedServers := IdentitySet new ]
]

{ #category : #public }
ZnServer class >> on: portNumber [
	"Instantiate a new listener on a given port,
	send #start to it to start listening.
	Delegate to the default server class."

	^ self defaultServerClass on: portNumber 
]

{ #category : #'system startup' }
ZnServer class >> register: server [
	"Arrange for server to be sent start/stop on system startUp/shutDown"
	
	self managedServers add: server
]

{ #category : #'system startup' }
ZnServer class >> shutDown: quiting [
	"Our system shutDown hook: stop all servers we manage"
	
	(quiting or: [ self alwaysRestart ])
		ifTrue: [
			self managedServers do: [ :each | each stop: false ] ]
]

{ #category : #public }
ZnServer class >> startDefaultOn: portNumber [
	"Start and return the default instance on a given port.
	Keep a reference to it in a class instance variable.
	If there was no previous default instance, create a new one.
	If there was a previous default instance, reuse it:
	if it was running stop and start it, effectively restarting it.
	Change the port if necessary.
	Starting the default server will register it automatically.
	Delegate to the default server class."

	^ self defaultServerClass startDefaultOn: portNumber 
]

{ #category : #public }
ZnServer class >> startOn: portNumber [
	"Instanciate and return a new listener on a given port and start listening.
	Delegate to the default server class."

	^ self defaultServerClass startOn: portNumber 
]

{ #category : #'system startup' }
ZnServer class >> startUp: resuming [
	"Our system startUp hook: start all servers we manage.
	We do this using deferred startup actions to allow normal error handling."

	(resuming or: [ self alwaysRestart ])
		ifTrue: [
			self environment 
				at: #SessionManager 
				ifPresent: [ :manager | 
					manager default currentSession addDeferredStartupAction: [ 
						self managedServers do: [ :each | each start ] ] ]
				ifAbsent: [ 
					Smalltalk addDeferredStartupAction: [ 
						self managedServers do: [ :each | each start ] ] ] ]
]

{ #category : #public }
ZnServer class >> stopDefault [
	"Stop the default instance, if any, and clear it.
	Return the stopped instance, if any.
	Delegate to the default server class."

	^ self defaultServerClass stopDefault 
]

{ #category : #'system startup' }
ZnServer class >> unregister: server [
	"No longer send server start/stop on system startUp/shutDown"
	
	self managedServers remove: server ifAbsent: [ ]
]

{ #category : #options }
ZnServer >> authenticator [
	"Return the optional authenticator,
	the object that will be sent #authenticateRequest:do:
	to authenticate or refuse the requests. 
	When authentication succeeds, the block should be executed,
	when authentication fails, a appropriate response should be returned.
	If there is no authenticator, all requests will pass"
	
	^ self optionAt: #authenticator ifAbsent: [ nil ]
]

{ #category : #options }
ZnServer >> authenticator: object [
	"Set the object that will be sent #authenticateRequest:do:
	to authenticate or refuse the requests. Can be nil.
	When authentication succeeds, the block should be executed,
	when authentication fails, a appropriate response should be returned"

	self optionAt: #authenticator put: object
]

{ #category : #options }
ZnServer >> bindingAddress [
	"Return the interface address we are (or will be) listening on.
	Nil means that we are (or will be) listening on all/any interfaces."
	
	^ self optionAt: #bindAddress ifAbsent: [ nil ] 
]

{ #category : #options }
ZnServer >> bindingAddress: address [
	"Set the interface address we will be listening on.
	Specify nil to listen on all/any interfaces, the default.
	Address must be a 4 element ByteArray, like #[127 0 0 1].
	Cannot be changed after the server is already running."
	
	self optionAt: #bindAddress put: address
]

{ #category : #options }
ZnServer >> debugMode [
	"Return whether we are in debug mode, the default is false."
	
	^ self optionAt: #debugMode ifAbsent: [ false ] 
]

{ #category : #options }
ZnServer >> debugMode: boolean [
	"Set my debug mode, the default being false.
	In debug mode, Smalltalk Error during #handleRequest: will raise a Debugger.
	When not in debug mode, a Smalltalk Error during #handleRequest: will result in an HTTP Server Error response."
	
	^ self optionAt: #debugMode put: boolean
]

{ #category : #options }
ZnServer >> defaultEncoder [
	"The default character encoder to use when none is set in a mime-type"
	
	^ self optionAt: #defaultEncoder ifAbsent: [ ZnDefaultCharacterEncoder value ] 
]

{ #category : #options }
ZnServer >> defaultEncoder: encoder [
	"Set the default character encoder to use when none is set in a mime-type"
	
	^ self optionAt: #defaultEncoder put: encoder asZnCharacterEncoder
]

{ #category : #options }
ZnServer >> delegate [
	"Return the optional delegate, 
	the object that will be sent #handleRequest: to handle a request and produce a response.
	The default delegate is ZnDefaultServerDelegate"
	
	^ self optionAt: #delegate ifAbsentPut: [ ZnDefaultServerDelegate new ]
]

{ #category : #options }
ZnServer >> delegate: object [
	"Set the delegate to object. Can be nil.
	This will be sent #handleRequest: to handle a request and produce a response"
	
	self optionAt: #delegate put: object
]

{ #category : #testing }
ZnServer >> isListening [
	"Return true when I have a valid server socket listening at the correct port"
	
	self subclassResponsibility 
]

{ #category : #testing }
ZnServer >> isRunning [
	"Return true when I am running"
	
	self subclassResponsibility 
]

{ #category : #accessing }
ZnServer >> localUrl [
	"Return a ZnUrl to access me."
	
	^ ZnUrl new
		scheme: self scheme;
		host: NetNameResolver loopBackName;
		port: self port;
		yourself
]

{ #category : #options }
ZnServer >> logServerErrorDetails [
	"Return whether we log server error details including a short strack trace, the default is true."
	
	^ self optionAt: #debugMode ifAbsent: [ true ] 
]

{ #category : #options }
ZnServer >> logServerErrorDetails: boolean [
	"Set whether we log server error details including a short strack trace, the default is true."
	
	self optionAt: #debugMode put: boolean
]

{ #category : #options }
ZnServer >> maximumEntitySize [
	"Return the maximum entity size in bytes that I will read from a stream before signalling ZnEntityTooLarge"
	
	^ self optionAt: #maximumEntitySize ifAbsent: [ ZnMaximumEntitySize value ]
]

{ #category : #options }
ZnServer >> maximumEntitySize: integer [
	"Set the maximum entity size in bytes that I will read from a stream before signalling ZnEntityTooLarge"
	
	^ self optionAt: #maximumEntitySize put: integer
]

{ #category : #options }
ZnServer >> maximumNumberOfDictionaryEntries [
	"Return the maximum number of entries allowed in ZnMutliValueDictionaries before signalling ZnTooManyDictionaryEntries. This protects us from overflow attacks."
		
	^ self
		optionAt: #maximumNumberOfDictionaryEntries
		ifAbsent: [ ZnMaximumNumberOfDictionaryEntries value ]
]

{ #category : #options }
ZnServer >> maximumNumberOfDictionaryEntries: anInteger [
	"Set the maximum number of entries allowed in ZnMutliValueDictionaries before signalling ZnTooManyDictionaryEntries. This protects us from overflow attacks."
	
	^ self
		optionAt: #maximumNumberOfDictionaryEntries
		put: anInteger
]

{ #category : #accessing }
ZnServer >> optionAt: key ifAbsent: block [
	"Return my option/settings stored under key.
	Execute block if I have no such option/setting.
	This is a generic interface, see my options protocol for specific usages."

	options ifNil: [ ^ block value ].
	^ options at: key ifAbsent: block
]

{ #category : #accessing }
ZnServer >> optionAt: key ifAbsentPut: block [
	"Return my option/settings stored under key.
	If I have no such option/setting, store the result of evaluating block as new value and return it.
	This is a generic interface, see my options protocol for specific usages."

	^ options at: key ifAbsentPut: block
]

{ #category : #accessing }
ZnServer >> optionAt: key put: value [
	"Set my option/setting identified by key to be value.
	This is a generic interface, see my options protocol for specific usages."
	
	options ifNil: [ options := Dictionary new ].
	options at: key put: value
]

{ #category : #options }
ZnServer >> port [
	"Return the integer port number we are (or will be) listening on"
	
	^ self optionAt: #port ifAbsent: [ 1701 ] 
]

{ #category : #options }
ZnServer >> port: integer [
	"Set the port number we will be listening on.
	Cannot be changed after the server is already running."
	
	(self isRunning and: [ self port ~= integer ]) 
		ifTrue: [ self error: 'Stop me before changing my port' ].
	self optionAt: #port put: integer
]

{ #category : #accessing }
ZnServer >> process [
	"Return the process that is running my main listening loop.
	Will be nil when I am not running"
	
	self subclassResponsibility 
]

{ #category : #options }
ZnServer >> reader [
	"Return a block that when given a stream reads an entity from it."
	
	^ self optionAt: #reader ifAbsentPut: [ [ :stream | ZnRequest readFrom: stream ] ]
]

{ #category : #options }
ZnServer >> reader: block [
	"Customize how entities are read from a stream, see #reader"
	
	self optionAt: #reader put: block
]

{ #category : #public }
ZnServer >> register [
	"Ask for start/stop to be sent to me on System startUp/shutDown"
	
	self class register: self
]

{ #category : #options }
ZnServer >> route [
	"Return the route of the server.
	This is a short identification string to be appended at the end of server session ids, separated by a dot.
	Routes are used by load balancers and proxies to correctly implement session affiinity or stickyness.
	The default is nil, meaning that no route has to be appended."
	
	^ self optionAt: #route ifAbsent: [ nil ]
]

{ #category : #options }
ZnServer >> route: object [
	"Set the route of the server.
	This is a short identification string to be appended at the end of server session ids, separated by a dot.
	Routes are used by load balancers and proxies to correctly implement session affiinity or stickyness.
	The default is nil, meaning that no route has to be appended."

	self optionAt: #route put: object
]

{ #category : #accessing }
ZnServer >> scheme [
	^ #http
]

{ #category : #accessing }
ZnServer >> serverSocket [
	"Return the server socket that I am using.
	Will be nil when I am not running"
	
	self subclassResponsibility 
]

{ #category : #options }
ZnServer >> serverUrl [
	"Return the explicitely set external server URL, if any. Defaults to nil."
	
	^ self optionAt: #serverUrl ifAbsent: [ nil ]
]

{ #category : #options }
ZnServer >> serverUrl: urlObject [
	"Set the explicit external server URL to urlObject. Defaults to nil.
	urlObject should be a ZnUrl or a String that parses correctly to one.
	See also #url."
	
	^ self optionAt: #serverUrl put: urlObject asZnUrl
]

{ #category : #accessing }
ZnServer >> sessionFor: request [
	"Bind an existing session to request or create a new session"
	
	^ self sessionManager sessionFor: request
]

{ #category : #accessing }
ZnServer >> sessionManager [
	"Return my session manager"

	^ sessionManager ifNil: [ sessionManager := ZnServerSessionManager new ]
]

{ #category : #public }
ZnServer >> start [
	"Start me. I will start listening on my port for incoming HTTP connections.
	If I am running, I will first stop and thus effectively restart"
	
	self subclassResponsibility
]

{ #category : #public }
ZnServer >> stop [
	"Stop me. I will stop listening on my port for incoming HTTP connections.
	Does nothing when I am not running"
	
	self stop: true 
]

{ #category : #public }
ZnServer >> stop: unregister [
	"Stop me. I will stop listening on my port for incoming HTTP connections.
	If unregister is true, unregister me from the list of managed instances.
	Does nothing when I am not running"
	
	self subclassResponsibility 
]

{ #category : #public }
ZnServer >> unregister [
	"Ask for start/stop to no longer be sent to me on System startUp/shutDown"

	self class unregister: self
]

{ #category : #accessing }
ZnServer >> url [
	"Return the base external URL (a new ZnUrl instance) to access me.
	This defaults to #localUrl but can be set explicitly using the #serverUrl option.
	Missing elements from #serverUrl are merged in from #localUrl."

	^ self serverUrl 
		ifNil: [ self localUrl ] 
		ifNotNil: [ :serverUrl | serverUrl inContextOf: self localUrl ]
]

{ #category : #options }
ZnServer >> useGzipCompressionAndChunking [
	"Return whether we should try to use gzip content encoding and chunked transfer encoding, the default is false."
	
	^ self optionAt: #useGzipCompressionAndChunking ifAbsent: [ false ] 
]

{ #category : #options }
ZnServer >> useGzipCompressionAndChunking: boolean [
	"Set whether we should try to use gzip content encoding and chunked transfer encoding, the default being false."
	
	self optionAt: #useGzipCompressionAndChunking put: boolean
]
