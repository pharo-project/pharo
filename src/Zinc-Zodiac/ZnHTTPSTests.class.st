"
ZnHTTPSTests.

These require that the ZnZodiacNetworkingUtils socket stream factory is enabled:

	ZnNetworkingUtils default: ZnZodiacNetworkingUtils new.
	
Part of Zinc HTTP Components
"
Class {
	#name : #ZnHTTPSTests,
	#superclass : #TestCase,
	#category : #'Zinc-Zodiac'
}

{ #category : #accessing }
ZnHTTPSTests class >> defaultTimeLimit [
	^10 seconds
]

{ #category : #testing }
ZnHTTPSTests class >> generateTestData: size [
	"self generateTestData: 1111"
	
	^ String 
		streamContents: [ :stream |
			stream << 'Transfer test file of size '; print: size; << ' bytes.'; lf.
			1 to: size do: [ :each |
				stream nextPutAll: 'Line '; print: each; lf ] ] 
		limitedTo: size 
]

{ #category : #testing }
ZnHTTPSTests class >> generateTestFiles [
	"self generateTestFiles"
	
	(Integer primesUpTo: 100) do: [ :each | | size |
		size := 1024 * each + each.
		FileStream fileNamed: ('test-', size asString, '.txt') do: [ :stream |
			stream nextPutAll: (self generateTestData: size) ] ]
]

{ #category : #testing }
ZnHTTPSTests >> disabledTestPayPal [
	| client |
	self ensureSocketStreamFactory.
	self isNativeSSLPluginPresent ifFalse: [ ^ self ].
	(client := ZnClient new) 
		get: 'https://www.paypal.com'.
	self assert: client isSuccess.
	self assert: (client contents includesSubstring: 'PayPal').
	client close
]

{ #category : #private }
ZnHTTPSTests >> doesNativeSSLPluginSupportSNI [
	"Return whether the SSL VM plugin supports SNI."
	
	^ [ ZdcPluginSSLSession new serverName; serverName: nil; destroy. true ]
			on: ZdcPluginMissing, PrimitiveFailed
			do: [ false ]
]

{ #category : #private }
ZnHTTPSTests >> ensureSocketStreamFactory [
	self 
		assert: ZnNetworkingUtils default secureSocketStreamClass notNil
		description: 'Please set a secure socket stream class in ZnNetworkingUtils default or switch to the ZnZodiacNetworkingUtils socket stream factory'
]

{ #category : #private }
ZnHTTPSTests >> isNativeSSLPluginPresent [
	"Return whether the SSL VM plugin can be instanciated and intialized."
	
	^ [ ZdcPluginSSLSession new destroy. true ]
			on: ZdcPluginMissing
			do: [ false ]
]

{ #category : #private }
ZnHTTPSTests >> runningOnWindows [
	"Pharo 3+"
	[ ^ OSPlatform current isWindows ] on: MessageNotUnderstood do: [ ].
	"Pharo 2-"
	[ ^ OSPlatform isWin32 ] on: MessageNotUnderstood do: [ ].
	"Give up"
	^ false
]

{ #category : #private }
ZnHTTPSTests >> runningOnWindowsInriaCI [
	| platform ci |
	self runningOnWindows
		ifFalse: [ ^ false ].
	^ [ 
	platform := OSPlatform current.
	ci := platform environment at: 'CI' ifAbsent: [ '' ].
	ci = 'ci.inria.fr' ]
		on: MessageNotUnderstood
		do: [ false ]
]

{ #category : #testing }
ZnHTTPSTests >> testAmazonAWS [
	| client |
	self ensureSocketStreamFactory.
	self isNativeSSLPluginPresent ifFalse: [ ^ self ].
	(client := ZnClient new) 
		get: 'https://aws.amazon.com/'.
	self assert: client isSuccess.
	self assert: (client contents includesSubstring: 'Amazon').
	client close
]

{ #category : #testing }
ZnHTTPSTests >> testGForceInria [
	| client |
	self ensureSocketStreamFactory.
	self isNativeSSLPluginPresent ifFalse: [ ^ self ].
	(client := ZnClient new)
		get: 'http://gforge.inria.fr/frs/?group_id=1299'.
	self assert: client isSuccess.
	self assert: (client contents includesSubstring: 'inria').
	client close
]

{ #category : #testing }
ZnHTTPSTests >> testGetPharoVersion [
	| client lastBuildVersion version |
	self ensureSocketStreamFactory.
	self isNativeSSLPluginPresent ifFalse: [ ^ self ].
	self runningOnWindowsInriaCI ifTrue: [ ^ self ].
	lastBuildVersion := (client := ZnClient new)
		beOneShot;
		get: 'https://ci.inria.fr/pharo/job/Pharo-6.0/lastSuccessfulBuild/api/xml?xpath=/*/fullDisplayName'.
	self assert: client isSuccess.
	self assert: (client response contentType matches: ZnMimeType applicationXml).
	self assert: client response contentLength > 0.
	self assert: lastBuildVersion notNil.
	self assert: lastBuildVersion isString.
	self assert: lastBuildVersion size > 0.
	version := (lastBuildVersion copyAfter: $>) copyUpTo: $<.
	self deny: version isEmpty
]

{ #category : #testing }
ZnHTTPSTests >> testGmailEncrypted [
	| client |
	self ensureSocketStreamFactory.
	self isNativeSSLPluginPresent ifFalse: [ ^ self ].  
	(client := ZnClient new) 
		get: 'https://www.gmail.com'.
	self assert: client isSuccess.
	self assert: (client contents includesSubstring: 'Google').    
	self assert: (client contents includesSubstring: 'mail'). 
	client close
]

{ #category : #testing }
ZnHTTPSTests >> testGoogleEncrypted [
	| client |
	self ensureSocketStreamFactory.
	self isNativeSSLPluginPresent ifFalse: [ ^ self ].
	(client := ZnClient new) 
		get: 'https://encrypted.google.com/search?q=Smalltalk'.
	self assert: client isSuccess.
	self assert: (client contents includesSubstring: 'Google').    
	self assert: (client contents includesSubstring: 'Smalltalk').
	client close
]

{ #category : #testing }
ZnHTTPSTests >> testRequestResponse [
	| query stream request response |
	self ensureSocketStreamFactory.
	self isNativeSSLPluginPresent ifFalse: [ ^ self ].
	query := 'Smalltalk'.
	stream := ZdcSecureSocketStream openConnectionToHostNamed: 'www.google.com' port: 443.
	[
		request := ZnRequest get: 'https://www.google.com/search?q=', query.
		stream connect.
		request writeOn: stream.
		stream flush.
		response := ZnResponse readFrom: stream.
	] ensure: [ stream close ].
	self assert: response isSuccess.
	self assert: (response contents includesSubstring: 'Google').   
	self assert: (response contents includesSubstring: 'Smalltalk').   

]

{ #category : #testing }
ZnHTTPSTests >> testSNIPage [
	| client |
	self ensureSocketStreamFactory.
	self isNativeSSLPluginPresent ifFalse: [ ^ self skip ].
	self doesNativeSSLPluginSupportSNI ifFalse: [ ^ self skip ].
	
	(client := ZnClient new) 
		get: 'https://www.mnot.net/blog/2014/05/09/if_you_can_read_this_youre_sniing'.

	self assert: client isSuccess.

	client close
]

{ #category : #testing }
ZnHTTPSTests >> testTransfers [
	self timeLimit: 300 seconds.
	self ensureSocketStreamFactory.
	self isNativeSSLPluginPresent ifFalse: [ ^ self ].
	
	(Integer primesUpTo: 100) do: [ :each | | size client |
		size := 1024 * each + each.
		(client := ZnClient new)
			https;
			host: 's3-eu-west-1.amazonaws.com';
			addPath: 'public-stfx-eu';
			addPath: ('test-', size asString, '.txt');
			get;
			close.
		self assert: client isSuccess.
		self assert: (client response contentType matches: ZnMimeType textPlain).
		self assert: client response contentLength equals: size.
		self 
			assert: client contents 
			equals: (self class generateTestData: size)	]
]

{ #category : #testing }
ZnHTTPSTests >> testTransfersSingleClient [
	| client |
	self ensureSocketStreamFactory.
	self isNativeSSLPluginPresent ifFalse: [ ^ self ].
	(client := ZnClient new)
		https;
		host: 's3-eu-west-1.amazonaws.com'.
	(Integer primesUpTo: 100) do: [ :each | | size |
		size := 1024 * each + each.
		client
			url: ('/public-stfx-eu/test-', size asString, '.txt');
			get.
		self assert: client isSuccess.
		self assert: (client response contentType matches: ZnMimeType textPlain).
		self assert: client response contentLength equals: size.
		self 
			assert: client contents 
			equals: (self class generateTestData: size)	].
	client close
]

{ #category : #testing }
ZnHTTPSTests >> testWikimedia [
	| client |
	self ensureSocketStreamFactory.
	self isNativeSSLPluginPresent ifFalse: [ ^ self ]. 
	(client := ZnClient new) 
		get: 'https://secure.wikimedia.org/'.
	self assert: client isSuccess.
	self assert: (client contents includesSubstring: 'Wikimedia').
	client close
]

{ #category : #private }
ZnHTTPSTests >> timeLimit: seconds [
	(TestCase canUnderstand: #timeLimit:)
		ifTrue: [ super timeLimit: seconds ]
]
