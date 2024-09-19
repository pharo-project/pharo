"
This class implements TCP/IP style network name lookup and translation facilities.

Attempt to keep track of whether there is a network available.
HaveNetwork	true if last attempt to contact the network was successful.
LastContact		Time of that contact (totalSeconds).
haveNetwork	returns true, false, or #expired.  True means there was contact in the last 30 minutes.  False means contact failed or was false last time we asked.  Get out of false state by making contact with a server in some way (FileList or updates).
"
Class {
	#name : 'NetNameResolver',
	#superclass : 'Object',
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
	#category : 'Network-Kernel-Base',
	#package : 'Network-Kernel',
	#tag : 'Base'
}

{ #category : 'lookups' }
NetNameResolver class >> addressForName: aString [
	^self addressForName: aString timeout: 60
]

{ #category : 'lookups' }
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

{ #category : 'address string utils' }
NetNameResolver class >> addressFromIPv6String: aString [
	"Return a ByteArray corresponding to an IPv6 address in text format"
	| stream headStream headBytes tailStream tailBytes hextets |
	headStream := WriteStream on: ByteArray new.
	tailStream := WriteStream on: ByteArray new.

	"Obtain a collection of the string hextets from the
	original IPv6 string. Any instance of a double colon (::)
	will be present in the list as an empty string."
	hextets := self splitIPv6HextetsOn: aString.
	stream := hextets readStream.
	(stream upTo: '') do: [ :hextet |
		headStream nextPutAll: (ByteArray readHexFrom: hextet) ].
	(stream upToEnd) do: [ :hextet |
		tailStream nextPutAll: (ByteArray readHexFrom: hextet) ].
	headBytes := headStream contents.
	tailBytes := tailStream contents.

	"If the total number of bytes is less than 16, that means
	we have a compressed range of 0-value hextets specified
	using the double colon (::). We add these at the end of
	the headBytes collection"
	(16 - (headBytes size + tailBytes size)) timesRepeat: [
		headBytes := headBytes,#[0] ].

	^ headBytes,tailBytes
]

{ #category : 'address string utils' }
NetNameResolver class >> addressFromString: addressString [
	"Return the internet address represented by the given string. The string should contain four positive decimal integers delimited by periods, commas, or spaces, where each integer represents one address byte. Return nil if the string is not a host address in an acceptable format."

	"NetNameResolver addressFromString: '1.2.3.4'"

	"NetNameResolver addressFromString: '1,2,3,4'"

	"NetNameResolver addressFromString: '1 2 3 4'"

	| newAddr s byte delimiter |
	"If the address string contains a colon, then we
	consider it an IPv6 address"
	(addressString includes: $:) ifTrue: [ ^ self addressFromIPv6String: addressString ].

	"Otherwise parse as a normal IPv4 string"
	newAddr := ByteArray new: 4.
	s := ReadStream on: addressString.
	s skipSeparators.
	1 to: 4 do: [ :i |
		byte := (self readDecimalByteFrom: s) ifNil: [ ^ nil ].
		newAddr at: i put: byte.
		i < 4 ifTrue: [
			delimiter := s next.
			(delimiter = $. or: [ delimiter = $, or: [ delimiter = $  ] ]) ifFalse: [ ^ nil ] ] ].
	^ newAddr
]

{ #category : 'private' }
NetNameResolver class >> findLongestIPv6ZerosIn: aCollection [
	"Given a collection of Integers, find the longest (or leftmost)
	range of contiguous consecutive zero values (if any).
	Respond with a size 2 array corresponding to the start
	and end index, or nil if there are no zero ranges at all"
	| active start end cursor ranges largest firstLargest |
	active := false. "Whether or not the beginning of some range has been found"
	start := 0.
	end := 0.
	ranges := OrderedCollection new.
	cursor := 1.
	[ cursor < aCollection size ] whileTrue: [
		| current next isConsecutive |
		current := aCollection at: cursor.
		next := aCollection at: cursor + 1.
		isConsecutive := (current = 0) and: [ next = 0].

		isConsecutive
			ifTrue: [
				active
					ifFalse: [
						"If the current values are consecutive zeros,
						and we are not active, set active to true and update
						the start and end values."
						start := cursor.
						end := cursor + 1.
						active := true ]
					ifTrue: [
						"If the current values are consecutive zeros,
						and we _are_ active, this means we need to just
						update the end"
						end := cursor + 1 ] ].


		"If we are active, but the values are _not_ consecutive,
		it means we've reached the end of a range"
		((active and: [ isConsecutive not ]) or: [ (active and: [ (cursor + 1) = aCollection size ])])
			ifTrue: [
				active := false.
				ranges add: { start. end }.
				start := 0.
				end := 0 ].

		"Increment the cursor"
		cursor := cursor + 1.
	].

	"If there are no zero ranges, return nil."
	ranges ifEmpty: [ ^ nil ].

	"Otherwise, return the longest range.
	If there are multiple ranges with the same size,
	return the first (leftmost) encountered."
	largest := 0.
	firstLargest := 1. "index of the first instance of the largest value, in case there are multiple with same size"
	(ranges collect: [ :range |
		(range last - range first )]) doWithIndex: [ :rangeSize :index |
			rangeSize > largest ifTrue: [
				largest := rangeSize.
				firstLargest := index ] ].

	^ ranges at: firstLargest
]

{ #category : 'class initialization' }
NetNameResolver class >> initialize [
	"Note: On the Mac, the name resolver is asynchronous, but can only handle one request at a time. On other platforms, such as Unix, the resolver is synchronous; a call to, say, the name lookup primitive will block all image processes until it returns."

	"Resolver Status Values"

	ResolverUninitialized := 0. "network is not initialized"
	ResolverReady := 1. "resolver idle, last request succeeded"
	ResolverBusy := 2. "lookup in progress"
	ResolverError := 3. "resolver idle, last request failed"

	DefaultHostName := ''.
	HaveNetwork := nil.
	ResolverMutex := Mutex new
]

{ #category : 'network initialization' }
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
		successful := (self primInitializeNetwork: semaIndex) isNotNil.
		HaveNetwork := nil.].
	^successful or: [NoNetworkError signal: 'failed network initialization']
]

{ #category : 'testing' }
NetNameResolver class >> isConnected [
	"Dirty, but avoids fixing the plugin bug"

	[ NetNameResolver addressForName: 'www.esug.org' ]
		on: NameLookupFailure
		do: [ :exception | ^ false ].
	^ true
]

{ #category : 'lookups' }
NetNameResolver class >> localAddressString [
	"Return a string representing the local host address as four decimal bytes delimited with decimal points."
	"NetNameResolver localAddressString"

	^ NetNameResolver stringFromAddress: NetNameResolver localHostAddress
]

{ #category : 'lookups' }
NetNameResolver class >> localHostAddress [
	"Return the local address of this host."
	"NetNameResolver localHostAddress"
	"On Mac the primitive is buggy and can return an empty IP address. Use a standard value in that case"

	| primAddress |

	self initializeNetwork.
	[ primAddress := self primLocalAddress ] on: PrimitiveFailed do: [ :err | primAddress := #[0 0 0 0] ].
	^ (primAddress = #[0 0 0 0] ifTrue: [ #[127 0 0 1] ] ifFalse: [ primAddress ]) asSocketAddress
]

{ #category : 'lookups' }
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

{ #category : 'lookups' }
NetNameResolver class >> loopBackAddress [
	^self addressForName: self loopBackName
]

{ #category : 'lookups' }
NetNameResolver class >> loopBackName [
	^'localhost'
]

{ #category : 'lookups' }
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

{ #category : 'private' }
NetNameResolver class >> nextSocketAddressInformation [

	| addrSize addr info |
	addrSize := self primGetAddressInfoSize.
	addrSize < 0 ifTrue: [^nil].
	addr := SocketAddress new: addrSize.
	self primGetAddressInfoResult: addr.
	info := SocketAddressInformation
		withSocketAddress: addr
		family: self primGetAddressInfoFamily
		type: self primGetAddressInfoType
		protocol: self primGetAddressInfoProtocol.
	self primGetAddressInfoNext.
	^info
]

{ #category : 'primitives' }
NetNameResolver class >> primAbortLookup [
	"Abort the current lookup operation, freeing the name resolver for the next query."

	<primitive: 'primitiveResolverAbortLookup' module: 'SocketPlugin'>
	self primitiveFailed
]

{ #category : 'primitives' }
NetNameResolver class >> primAddressLookupResult [
	"Return the host name found by the last host address lookup. Returns nil if the last lookup was unsuccessful."

	<primitive: 'primitiveResolverAddressLookupResult' module: 'SocketPlugin'>
	self primitiveFailed
]

{ #category : 'primitives' }
NetNameResolver class >> primGetAddressInfoFamily [

	<primitive: 'primitiveResolverGetAddressInfoFamily' module: 'SocketPlugin'>
	self primitiveFailed
]

{ #category : 'primitives' }
NetNameResolver class >> primGetAddressInfoHost: hostName service: servName flags: flags family: family type: type protocol: protocol [

	<primitive: 'primitiveResolverGetAddressInfo' module: 'SocketPlugin'>
	self primitiveFailed
]

{ #category : 'primitives' }
NetNameResolver class >> primGetAddressInfoNext [

	<primitive: 'primitiveResolverGetAddressInfoNext' module: 'SocketPlugin'>
	self primitiveFailed
]

{ #category : 'primitives' }
NetNameResolver class >> primGetAddressInfoProtocol [

	<primitive: 'primitiveResolverGetAddressInfoProtocol' module: 'SocketPlugin'>
	self primitiveFailed
]

{ #category : 'primitives' }
NetNameResolver class >> primGetAddressInfoResult: socketAddress [

	<primitive: 'primitiveResolverGetAddressInfoResult' module: 'SocketPlugin'>
	self primitiveFailed
]

{ #category : 'primitives' }
NetNameResolver class >> primGetAddressInfoSize [

	<primitive: 'primitiveResolverGetAddressInfoSize' module: 'SocketPlugin'>
	self primitiveFailed
]

{ #category : 'primitives' }
NetNameResolver class >> primGetAddressInfoType [

	<primitive: 'primitiveResolverGetAddressInfoType' module: 'SocketPlugin'>
	self primitiveFailed
]

{ #category : 'primitives' }
NetNameResolver class >> primGetNameInfoHostResult: aString [

 	<primitive: 'primitiveResolverGetNameInfoHostResult' module: 'SocketPlugin'>
 	self primitiveFailed
]

{ #category : 'primitives' }
NetNameResolver class >> primGetNameInfoHostSize [

 	<primitive: 'primitiveResolverGetNameInfoHostSize' module: 'SocketPlugin'>
 	self primitiveFailed
]

{ #category : 'primitives' }
NetNameResolver class >> primGetNameInfoServiceResult: aString [

 	<primitive: 'primitiveResolverGetNameInfoServiceResult' module: 'SocketPlugin'>
 	self primitiveFailed
]

{ #category : 'primitives' }
NetNameResolver class >> primGetNameInfoServiceSize [

 	<primitive: 'primitiveResolverGetNameInfoServiceSize' module: 'SocketPlugin'>
 	self primitiveFailed
]

{ #category : 'primitives' }
NetNameResolver class >> primHostNameResult: aString [

 	<primitive: 'primitiveResolverHostNameResult' module: 'SocketPlugin'>
 	self primitiveFailed
]

{ #category : 'primitives' }
NetNameResolver class >> primHostNameSize [

 	<primitive: 'primitiveResolverHostNameSize' module: 'SocketPlugin'>
 	self primitiveFailed
]

{ #category : 'network initialization' }
NetNameResolver class >> primInitializeNetwork: resolverSemaIndex [
	"Initialize the network drivers on platforms that need it, such as the Macintosh, and return nil if network initialization failed or the reciever if it succeeds. Since mobile computers may not always be connected to a network, this method should NOT be called automatically at startup time; rather, it should be called when first starting a networking application. It is a noop if the network driver has already been initialized. If non-zero, resolverSemaIndex is the index of a VM semaphore to be associated with the network name resolver. This semaphore will be signalled when the resolver status changes, such as when a name lookup query is completed."
	"Note: some platforms (e.g., Mac) only allow only one name lookup query at a time, so a manager process should be used to serialize resolver lookup requests."

	<primitive: 'primitiveInitializeNetwork' module: 'SocketPlugin'>
	^ nil  "return nil if primitive fails"
]

{ #category : 'primitives' }
NetNameResolver class >> primLocalAddress [
	"Return the local address of this host."

	<primitive: 'primitiveResolverLocalAddress' module: 'SocketPlugin'>
	self primitiveFailed
]

{ #category : 'primitives' }
NetNameResolver class >> primNameLookupResult [
	"Return the host address found by the last host name lookup. Returns nil if the last lookup was unsuccessful."

	<primitive: 'primitiveResolverNameLookupResult' module: 'SocketPlugin'>
	self primitiveFailed
]

{ #category : 'primitives' }
NetNameResolver class >> primNameResolverError [
	"Return an integer reflecting the error status of the last network name resolver request. Zero means no error."

	<primitive: 'primitiveResolverError' module: 'SocketPlugin'>
	self primitiveFailed
]

{ #category : 'primitives' }
NetNameResolver class >> primNameResolverStatus [
	"Return an integer reflecting the status of the network name resolver. For a list of possible values, see the comment in the 'initialize' method of this class."

	<primitive: 'primitiveResolverStatus' module: 'SocketPlugin'>
	self primitiveFailed
]

{ #category : 'primitives' }
NetNameResolver class >> primStartLookupOfAddress: hostAddr [
	"Look up the given host address in the Domain Name Server to find its name. This call is asynchronous. To get the results, wait for it to complete or time out and then use primAddressLookupResult."

	<primitive: 'primitiveResolverStartAddressLookup' module: 'SocketPlugin'>
	self primitiveFailed
]

{ #category : 'primitives' }
NetNameResolver class >> primStartLookupOfName: hostName [
	"Look up the given host name in the Domain Name Server to find its address. This call is asynchronous. To get the results, wait for it to complete or time out and then use primNameLookupResult."

	<primitive: 'primitiveResolverStartNameLookup' module: 'SocketPlugin'>
	self primitiveFailed
]

{ #category : 'private' }
NetNameResolver class >> putIPv6HexIntegers: aCollection on: aWriteStream [
	"Given a collection of Integers, convert them to
	hex strings and join them with the ':' separator
	on the given write stream"
	aCollection
		do: [ :integer |
			aWriteStream nextPutAll: (integer printStringBase: 16) asLowercase ]
		separatedBy: [ aWriteStream nextPut: $: ]
]

{ #category : 'private' }
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

{ #category : 'lookups' }
NetNameResolver class >> resolverError [
	^self primNameResolverError
]

{ #category : 'private' }
NetNameResolver class >> resolverMutex [
	"This must have been initialized by class initialization.
	If a failure occurs due to mutex not being properly initialized, do NOT solve it by lazy initialization, or you WILLl introduce a race condition"
	^ResolverMutex
]

{ #category : 'lookups' }
NetNameResolver class >> resolverStatus [
	^self primNameResolverStatus
]

{ #category : 'private' }
NetNameResolver class >> splitIPv6HextetsOn: aString [
	"Respond with a collection of hextet strings extracted
	from a single string in IPv6 format. These are separated
	by the $: character.
	We avoid using #splitOn: or #splitBy: since they
	are not consistent across Smalltalks"
	| stream result |
	result := OrderedCollection new.
	stream := aString readStream.
	[ stream atEnd ] whileFalse: [
		"Pad any valid hextets so that they are each length 4,
		ie they have the leading zeros. This is so that ByteArray class >> #readFromHex:
		will work properly -- it does not correctly parse hextets without the leading zeros."
		| next |
		next := stream upTo: $:.
		(next = '')
			ifTrue: [ result add: next ]
			ifFalse: [
				(4 - next size) timesRepeat: [
					next := '0',next ].
				result add: next ] ].

	^ result
]

{ #category : 'address string utils' }
NetNameResolver class >> stringFromAddress: addr [
	"Return a string representing the given host address as four decimal bytes delimited with decimal points."
	"NetNameResolver stringFromAddress: NetNameResolver localHostAddress"

	| s |
	(addr isKindOf: SocketAddress) ifTrue: [^addr printString copyUpTo: $( ].
	"If the incoming addr is a size 16 ByteArray, we assume it is
	representing an IPv6 address."
	(addr size = 16) ifTrue: [ ^ self stringFromIPv6Address: addr ].

	"Otherwise write out in IPv4 format"
	s := WriteStream on: ''.
	1 to: 3 do: [ :i | (addr at: i) printOn: s. s nextPut: $.].
	(addr at: 4) printOn: s.
	^ s contents
]

{ #category : 'address string utils' }
NetNameResolver class >> stringFromIPv6Address: aByteArray [
	"Respond with a correctly formatted IPv6 string
	parsed from the incoming ByteArray.
	Note the 'compressed-zero' rule for IPv6 address strings:
	the longest contiguous range of two or more consecutive
	0 values can be compressed to '::'.
	If there are two ranges of the same size, use the leftmost"
	| readStream integerCollection indices writeStream |

	"If the ByteArray as an Integer is zero (ie, it's all zeros),
	then just return the double colon"
	aByteArray asInteger = 0 ifTrue: [ ^ '::' ].

	readStream := aByteArray readStream.
	integerCollection := OrderedCollection new.
	[ readStream atEnd ] whileFalse: [
		integerCollection add: (readStream next: 2) asInteger ].

	"Find the start and end indices of the longest
	contiguous set of zeroes, if any"
	writeStream := WriteStream on: String new.
	indices := self findLongestIPv6ZerosIn: integerCollection.
	indices ifNil: [
		self putIPv6HexIntegers: integerCollection on: writeStream.
		^ writeStream contents ].

	"If the start index of the zeros is 1, then there are contiguous
	zeros at the beginning of the string"
	(indices first = 1) ifTrue: [
		writeStream nextPutAll: '::'.
		self
			putIPv6HexIntegers:  (integerCollection copyFrom: (indices second + 1) to: integerCollection size)
			on: writeStream.
		^ writeStream contents ].

	"If the end index of the zeros is the same as the size
	of the collection of integers, then there are contiguous
	zeros at the end of the string."
	(indices second = integerCollection size)
		ifTrue: [
			self
				putIPv6HexIntegers: (integerCollection copyFrom: 1 to: (indices first - 1))
				on: writeStream.
			writeStream nextPutAll: '::'.
			^ writeStream contents ].

	"Otherwise, there is a range of zeros somewhere in the middle
	of the string that should get compressed. Write normal strings for
	each side out of the range of the indices, then join them by the
	compression string (::)"
	self
		putIPv6HexIntegers: (integerCollection copyFrom: 1 to: (indices first - 1))
		on: writeStream.
	writeStream nextPutAll: '::'.
	self
		putIPv6HexIntegers: (integerCollection copyFrom: (indices second + 1) to: integerCollection size)
		on: writeStream.
	^ writeStream contents
]

{ #category : 'private' }
NetNameResolver class >> waitForCompletionUntil: deadline [
	"Wait up to the given number of seconds for the resolver to be ready to accept a new request. Return true if the resolver is ready, false if the network is not initialized or the resolver does not become free within the given time period."

	| status |
	status := self waitForResolverNonBusyUntil: deadline.
	^ status = ResolverReady
		ifTrue: [ true ]
		ifFalse: [ status = ResolverBusy ifTrue: [ self primAbortLookup ].
			false ]
]

{ #category : 'private' }
NetNameResolver class >> waitForResolverNonBusyUntil: deadline [
	"Wait up to the given number of seconds for the resolver to be non busy.
	Return the resolver state."

	| status passed |
	status := self resolverStatus.
	[ status = ResolverBusy and: [ (passed := Time millisecondsSince: deadline key) < deadline value] ]
		whileTrue: [
			"wait for resolver to be available"
			ResolverSemaphore waitTimeoutMilliseconds: (deadline value - passed).
			status := self resolverStatus ].
	^ status
]

{ #category : 'private' }
NetNameResolver class >> waitForResolverReadyUntil: deadline [
	"Wait up to the given number of seconds for the resolver to be ready to accept a new request. Return true if the resolver is not busy, false if the network is not initialized or the resolver does not become free within the given time period."

	| status |
	status := self resolverStatus.
	status = ResolverUninitialized ifTrue: [^ false].
	status := self waitForResolverNonBusyUntil: deadline.
	^ status ~= ResolverBusy
]
