"
This class implements TCP/IP style network name lookup and translation facilities.

Attempt to keep track of whether there is a network available.
HaveNetwork	true if last attempt to contact the network was successful.
LastContact		Time of that contact (totalSeconds).
haveNetwork	returns true, false, or #expired.  True means there was contact in the last 30 minutes.  False means contact failed or was false last time we asked.  Get out of false state by making contact with a server in some way (FileList or updates).
"
Class {
	#name : #NetNameResolver,
	#superclass : #Object,
	#classVars : [
		'DefaultHostName',
		'HaveNetwork',
		'ResolverBusy',
		'ResolverError',
		'ResolverMutex',
		'ResolverReady',
		'ResolverSemaphore',
		'ResolverUninitialized'
	],
	#category : #'Network-Kernel-Base'
}

{ #category : #lookups }
NetNameResolver class >> addressForName: aString [
	^self addressForName: aString timeout: 60
]

{ #category : #lookups }
NetNameResolver class >> addressForName: hostName timeout: secs [
	"Look up the given host name and return its address. Return nil if the address is not found in the given number of seconds."
	"NetNameResolver addressForName: 'create.ucsb.edu' timeout: 30"
	"NetNameResolver addressForName: '100000jobs.de' timeout: 30"
	"NetNameResolver addressForName: '1.7.6.4' timeout: 30"
	"NetNameResolver addressForName: '' timeout: 30"

	| deadline result |
	self initializeNetwork.
	"check if this is a valid numeric host address (e.g. 1.2.3.4)"
	result := self addressFromString: hostName.
	result ifNotNil: [ ^ result asSocketAddress ].

	"Look up a host name, including ones that start with a digit (e.g. 100000jobs.de or www.pharo-project.org)"
	deadline := Time millisecondClockValue -> (secs * 1000).
	"Protect the execution of this block, as the ResolverSemaphore is used for both parts of the transaction."
	self resolverMutex
		critical: [
			(self waitForResolverReadyUntil: deadline)
				ifTrue: [
					self primStartLookupOfName: hostName.
					(self waitForCompletionUntil: deadline)
						ifTrue: [ result := self primNameLookupResult ]
						ifFalse: [ ^ NameLookupFailure signalFor: hostName ] ]
				ifFalse: [ ^ NameLookupFailure signalFor: hostName ] ].
	^ result asSocketAddress
]

{ #category : #'address string utils' }
NetNameResolver class >> addressFromString: addressString [ 
	"Return the internet address represented by the given string. The string should contain four positive decimal integers delimited by periods, commas, or spaces, where each integer represents one address byte. Return nil if the string is not a host address in an acceptable format."
	"NetNameResolver addressFromString: '1.2.3.4'"
	"NetNameResolver addressFromString: '1,2,3,4'"
	"NetNameResolver addressFromString: '1 2 3 4'"
	| newAddr s byte delimiter |
	newAddr := ByteArray new: 4.
	s := addressString readStream.
	s skipSeparators.
	1 
		to: 4
		do: 
			[ :i | 
			byte := self readDecimalByteFrom: s.
			byte ifNil: [ ^ nil ].
			newAddr 
				at: i
				put: byte.
			i < 4 ifTrue: 
				[ delimiter := s next.
				(delimiter = $. or: [ delimiter = $, or: [ delimiter = $  ] ]) ifFalse: [ ^ nil ] ] ].
	^ newAddr
]

{ #category : #initialization }
NetNameResolver class >> initialize [
	"NetNameResolver initialize"
	"Note: On the Mac, the name resolver is asynchronous (i.e., Squeak can do other things while it is working), but can only handle one request at a time. On other platforms, such as Unix, the resolver is synchronous; a call to, say, the name lookup primitive will block all Squeak processes until it returns."

	"Resolver Status Values"
	ResolverUninitialized := 0.	"network is not initialized"
	ResolverReady := 1.			"resolver idle, last request succeeded"
	ResolverBusy := 2.			"lookup in progress"
	ResolverError := 3.			"resolver idle, last request failed"

	DefaultHostName := ''.
	HaveNetwork := nil.
	ResolverMutex := Mutex new
	

]

{ #category : #'network initialization' }
NetNameResolver class >> initializeNetwork [
	"Initialize the network drivers and record the semaphore to be used by the resolver. Do nothing if the network is already initialized.."
	"NetNameResolver initializeNetwork"

	| successful  sema|
	 "network is already initialized"
	(self resolverStatus = ResolverUninitialized)
		ifFalse: [^true].
	"No real message sends allowed in the atomic check, so pre-create a semaphore"
	sema := Semaphore forMutualExclusion.
	"Atomically check if another process is in the progress of initializing network.
	If so, block untill it is done and retry, otherwise start setting it up.
	Not doing so could lead to
	- External semaphore leakage (if we both try to set up simultaneously)
	- Returning an incorrect result (if we return a value independent of whether the other process was successful)"
	HaveNetwork == nil ifTrue: [HaveNetwork := sema].
	"Then, enter critical section where other process has initialized, or we need to do it."
	HaveNetwork critical: [ |semaIndex| 
		"If other process initialized while we were blocked, retry to see if it were successful"
		HaveNetwork ifNil: [^self initializeNetwork].
		"If the network has previously been initialized, but now unavailable, we need to unregister semaphore"
		ResolverSemaphore ifNotNil: [Smalltalk unregisterExternalObject: ResolverSemaphore].
		ResolverSemaphore := Semaphore new.
		semaIndex := Smalltalk registerExternalObject: ResolverSemaphore.
		successful := (self primInitializeNetwork: semaIndex) notNil.
		HaveNetwork := nil.].	
	^successful or: [NoNetworkError signal: 'failed network initialization']
	

]

{ #category : #testing }
NetNameResolver class >> isConnected [
	"Dirty, but avoids fixing the plugin bug"

	[ NetNameResolver addressForName: 'www.esug.org' ]
		on: NameLookupFailure
		do: [ :exception | ^ false ].
	^ true
]

{ #category : #lookups }
NetNameResolver class >> localAddressString [
	"Return a string representing the local host address as four decimal bytes delimited with decimal points."
	"NetNameResolver localAddressString"

	^ NetNameResolver stringFromAddress: NetNameResolver localHostAddress

]

{ #category : #lookups }
NetNameResolver class >> localHostAddress [
	"Return the local address of this host."
	"NetNameResolver localHostAddress"
	<todo>
	"On Mac the primitive is buggy and can return an empty IP address. Use a standard value in that case"
		
	| primAddress |

	self initializeNetwork.
	primAddress := self primLocalAddress.
	^ (primAddress = #[0 0 0 0] ifTrue: [ #[127 0 0 1] ] ifFalse: [ primAddress ]) asSocketAddress
]

{ #category : #lookups }
NetNameResolver class >> localHostName [
	"Return the local name of this host."

	"NetNameResolver localHostName"

	^ [ | hostName |
	self initializeNetwork.
	hostName := String new: self primHostNameSize.
	self primHostNameResult: hostName.
	hostName ]
		on: PrimitiveFailed
		do: [ self loopBackName ]
]

{ #category : #lookups }
NetNameResolver class >> loopBackAddress [
	^self addressForName: self loopBackName
]

{ #category : #lookups }
NetNameResolver class >> loopBackName [
	^'localhost'
]

{ #category : #lookups }
NetNameResolver class >> nameForAddress: hostAddress timeout: secs [
	"Look up the given host address and return its name. Return nil if the lookup fails or is not completed in the given number of seconds. Depends on the given host address being known to the gateway, which may not be the case for dynamically allocated addresses."
	"NetNameResolver
		nameForAddress: (NetNameResolver addressFromString: '128.111.92.2')
		timeout: 30"

	| deadline result |
	self initializeNetwork.
	deadline := Time millisecondClockValue -> (secs * 1000).
	"Protect the execution of this block, as the ResolverSemaphore is used for both parts of the transaction."
	self resolverMutex
		critical: [
			result := (self waitForResolverReadyUntil: deadline)
				ifTrue: [
					self primStartLookupOfAddress: hostAddress.
					(self waitForCompletionUntil: deadline)
						ifTrue: [self primAddressLookupResult]
						ifFalse: [nil]]
				ifFalse: [nil]].
	^result

]

{ #category : #primitives }
NetNameResolver class >> primAbortLookup [
	"Abort the current lookup operation, freeing the name resolver for the next query."

	<primitive: 'primitiveResolverAbortLookup' module: 'SocketPlugin'>
	self primitiveFailed

]

{ #category : #primitives }
NetNameResolver class >> primAddressLookupResult [
	"Return the host name found by the last host address lookup. Returns nil if the last lookup was unsuccessful."

	<primitive: 'primitiveResolverAddressLookupResult' module: 'SocketPlugin'>
	self primitiveFailed

]

{ #category : #primitives }
NetNameResolver class >> primGetAddressInfoFamily [

	<primitive: 'primitiveResolverGetAddressInfoFamily' module: 'SocketPlugin'>
	self primitiveFailed
]

{ #category : #primitives }
NetNameResolver class >> primGetAddressInfoHost: hostName service: servName flags: flags family: family type: type protocol: protocol [

	<primitive: 'primitiveResolverGetAddressInfo' module: 'SocketPlugin'>
	self primitiveFailed
]

{ #category : #primitives }
NetNameResolver class >> primGetAddressInfoNext [

	<primitive: 'primitiveResolverGetAddressInfoNext' module: 'SocketPlugin'>
	self primitiveFailed
]

{ #category : #primitives }
NetNameResolver class >> primGetAddressInfoProtocol [

	<primitive: 'primitiveResolverGetAddressInfoProtocol' module: 'SocketPlugin'>
	self primitiveFailed
]

{ #category : #primitives }
NetNameResolver class >> primGetAddressInfoResult: socketAddress [

	<primitive: 'primitiveResolverGetAddressInfoResult' module: 'SocketPlugin'>
	self primitiveFailed
]

{ #category : #primitives }
NetNameResolver class >> primGetAddressInfoSize [

	<primitive: 'primitiveResolverGetAddressInfoSize' module: 'SocketPlugin'>
	self primitiveFailed
]

{ #category : #primitives }
NetNameResolver class >> primGetAddressInfoType [

	<primitive: 'primitiveResolverGetAddressInfoType' module: 'SocketPlugin'>
	self primitiveFailed
]

{ #category : #primitives }
NetNameResolver class >> primGetNameInfoHostResult: aString [
 
 	<primitive: 'primitiveResolverGetNameInfoHostResult' module: 'SocketPlugin'>
 	self primitiveFailed
]

{ #category : #primitives }
NetNameResolver class >> primGetNameInfoHostSize [

 	<primitive: 'primitiveResolverGetNameInfoHostSize' module: 'SocketPlugin'>
 	self primitiveFailed
]

{ #category : #primitives }
NetNameResolver class >> primGetNameInfoServiceResult: aString [
 
 	<primitive: 'primitiveResolverGetNameInfoServiceResult' module: 'SocketPlugin'>
 	self primitiveFailed
]

{ #category : #primitives }
NetNameResolver class >> primGetNameInfoServiceSize [
 
 	<primitive: 'primitiveResolverGetNameInfoServiceSize' module: 'SocketPlugin'>
 	self primitiveFailed
]

{ #category : #primitives }
NetNameResolver class >> primHostNameResult: aString [
 
 	<primitive: 'primitiveResolverHostNameResult' module: 'SocketPlugin'>
 	self primitiveFailed
]

{ #category : #primitives }
NetNameResolver class >> primHostNameSize [
 
 	<primitive: 'primitiveResolverHostNameSize' module: 'SocketPlugin'>
 	self primitiveFailed
]

{ #category : #'network initialization' }
NetNameResolver class >> primInitializeNetwork: resolverSemaIndex [
	"Initialize the network drivers on platforms that need it, such as the Macintosh, and return nil if network initialization failed or the reciever if it succeeds. Since mobile computers may not always be connected to a network, this method should NOT be called automatically at startup time; rather, it should be called when first starting a networking application. It is a noop if the network driver has already been initialized. If non-zero, resolverSemaIndex is the index of a VM semaphore to be associated with the network name resolver. This semaphore will be signalled when the resolver status changes, such as when a name lookup query is completed."
	"Note: some platforms (e.g., Mac) only allow only one name lookup query at a time, so a manager process should be used to serialize resolver lookup requests."

	<primitive: 'primitiveInitializeNetwork' module: 'SocketPlugin'>
	^ nil  "return nil if primitive fails"

]

{ #category : #primitives }
NetNameResolver class >> primLocalAddress [
	"Return the local address of this host."

	<primitive: 'primitiveResolverLocalAddress' module: 'SocketPlugin'>
	self primitiveFailed

]

{ #category : #primitives }
NetNameResolver class >> primNameLookupResult [
	"Return the host address found by the last host name lookup. Returns nil if the last lookup was unsuccessful."

	<primitive: 'primitiveResolverNameLookupResult' module: 'SocketPlugin'>
	self primitiveFailed

]

{ #category : #primitives }
NetNameResolver class >> primNameResolverError [
	"Return an integer reflecting the error status of the last network name resolver request. Zero means no error."

	<primitive: 'primitiveResolverError' module: 'SocketPlugin'>
	self primitiveFailed

]

{ #category : #primitives }
NetNameResolver class >> primNameResolverStatus [
	"Return an integer reflecting the status of the network name resolver. For a list of possible values, see the comment in the 'initialize' method of this class."

	<primitive: 'primitiveResolverStatus' module: 'SocketPlugin'>
	self primitiveFailed

]

{ #category : #primitives }
NetNameResolver class >> primStartLookupOfAddress: hostAddr [
	"Look up the given host address in the Domain Name Server to find its name. This call is asynchronous. To get the results, wait for it to complete or time out and then use primAddressLookupResult."

	<primitive: 'primitiveResolverStartAddressLookup' module: 'SocketPlugin'>
	self primitiveFailed

]

{ #category : #primitives }
NetNameResolver class >> primStartLookupOfName: hostName [
	"Look up the given host name in the Domain Name Server to find its address. This call is asynchronous. To get the results, wait for it to complete or time out and then use primNameLookupResult."

	<primitive: 'primitiveResolverStartNameLookup' module: 'SocketPlugin'>
	self primitiveFailed

]

{ #category : #lookups }
NetNameResolver class >> promptUserForHostAddress [
	"Ask the user for a host name and return its address."
	"NetNameResolver promptUserForHostAddress"

	^ NetNameResolver promptUserForHostAddressDefault: ''

]

{ #category : #lookups }
NetNameResolver class >> promptUserForHostAddressDefault: defaultName [
	"Ask the user for a host name and return its address. If the default name is the empty string, use the last host name as the default."

	"NetNameResolver promptUserForHostAddressDefault: ''"

	| default hostName serverAddr |
	default := defaultName isEmpty
		ifTrue: [ DefaultHostName ]
		ifFalse: [ defaultName ].
	hostName := UIManager default request: 'Host name or address?' initialAnswer: default.
	hostName isEmptyOrNil
		ifTrue: [ ^ 0 ].
	serverAddr := NetNameResolver addressForName: hostName timeout: 15.
	hostName size > 0
		ifTrue: [ DefaultHostName := hostName ].
	^ serverAddr
]

{ #category : #private }
NetNameResolver class >> readDecimalByteFrom: aStream [
	"Read a positive, decimal integer from the given stream. Stop when a non-digit or end-of-stream is encountered. Return nil if stream is not positioned at a decimal digit or if the integer value read exceeds 255.
JMM - 000503 fixed didn't work correctly"

	| digitSeen value digit |
	digitSeen := false.
	value := 0.
	[aStream atEnd] whileFalse: 
		[digit := aStream next digitValue.
		(digit < 0 or: [digit > 9]) ifTrue: [
			aStream skip: -1.
			(digitSeen not or: [value > 255]) ifTrue: [^ nil].
			^ value].
		digitSeen := true.
		value := (value * 10) + digit].
	(digitSeen not or: [value > 255]) ifTrue: [^ nil].
	^ value

]

{ #category : #lookups }
NetNameResolver class >> resolverError [
	^self primNameResolverError

]

{ #category : #private }
NetNameResolver class >> resolverMutex [
	"This must have been initialized by class initialization. 
	If a failure occurs due to mutex not being properly initialized, do NOT solve it by lazy initialization, or you WILLl introduce a race condition"
	^ResolverMutex
]

{ #category : #lookups }
NetNameResolver class >> resolverStatus [
	^self primNameResolverStatus

]

{ #category : #'address string utils' }
NetNameResolver class >> stringFromAddress: addr [
	"Return a string representing the given host address as four decimal bytes delimited with decimal points."
	"NetNameResolver stringFromAddress: NetNameResolver localHostAddress"

	| s |
	s := String new writeStream.
	1 to: 3 do: [ :i | (addr at: i) printOn: s. s nextPut: $.].
	(addr at: 4) printOn: s.
	^ s contents

]

{ #category : #private }
NetNameResolver class >> waitForCompletionUntil: deadline [
	"Wait up to the given number of seconds for the resolver to be ready to accept a new request. Return true if the resolver is ready, false if the network is not initialized or the resolver does not become free within the given time period."

	| status |
	status := self waitForResolverNonBusyUntil: deadline.
	status = ResolverReady
		ifTrue: [^ true]
		ifFalse: [
			status = ResolverBusy ifTrue: [self primAbortLookup].
			^ false].

]

{ #category : #private }
NetNameResolver class >> waitForResolverNonBusyUntil: deadline [
	"Wait up to the given number of seconds for the resolver to be non busy.
	Return the resolver state."

	| status passed |
	status := self resolverStatus.
	[ status = ResolverBusy and: [ (passed := Time millisecondsSince: deadline key) < deadline value] ]
		whileTrue: [
			"wait for resolver to be available"
			ResolverSemaphore waitTimeoutMSecs: (deadline value - passed).
			status := self resolverStatus ].
	^ status
]

{ #category : #private }
NetNameResolver class >> waitForResolverReadyUntil: deadline [
	"Wait up to the given number of seconds for the resolver to be ready to accept a new request. Return true if the resolver is not busy, false if the network is not initialized or the resolver does not become free within the given time period."

	| status |
	status := self resolverStatus.
	status = ResolverUninitialized ifTrue: [^ false].
	status := self waitForResolverNonBusyUntil: deadline.
	^ status ~= ResolverBusy

]
