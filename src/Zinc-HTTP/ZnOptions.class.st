"
I am ZnOptions, a object that holds key/value options for configuration, parameterization and settings.

Option objects contain key/value pairs and inherit from a parent.
They can be writeable or readonly.

Clients typically refer to option using the ZnCurrentOptions dynamic variable.

	ZnCurrentOptions at: #myOption
	
The top level, global default is typically on my class side marked by the <znOption> pragma.

I hold the #globalDefault options.

Options can be cloned to make them writeable.

Options can changed and activated using #during:

ZnOptions globalDefault clone
	at: #myOption put: 'newValue';
	during: [ ZnCurrentOptions at: #myOption ].

ZnClient and ZnServer instances hold and apply their own local options.
They use or #conditionallyDuring: to allow an enclosing #during: to take precedence.

"
Class {
	#name : #ZnOptions,
	#superclass : #Object,
	#instVars : [
		'options',
		'parent',
		'writable'
	],
	#classVars : [
		'globalDefault'
	],
	#category : #'Zinc-HTTP-Support'
}

{ #category : #utilities }
ZnOptions class >> definitionOf: key [
	| implementors options |
	implementors := key implementors.
	options := implementors select: [ :method |
		method method pragmas
			anySatisfy: [ :pragma | pragma selector = #znOption ] ].
	options ifEmpty: [ self error: 'Not option definition found for ' , key printString ].
	options size > 1 ifTrue: [ self error: 'Multiple conflicting option definitions found for ' , key printString ].
	^ options first method
]

{ #category : #accessing }
ZnOptions class >> globalDefault [
	^ globalDefault ifNil: [ globalDefault := self root clone ]
]

{ #category : #'class initialization' }
ZnOptions class >> initialize [
	"Changed at 2021-12-13"

	self resetGlobalDefault
]

{ #category : #options }
ZnOptions class >> maximumEntitySize [
	"The maximum entity size in bytes that can read from a stream before ZnEntityTooLarge is signalled.
	Entities are the resources transferred by the HTTP procotol. Malicious parties could hurt a client or
	server by sending artificially large payloads. Setting a maximum is a protection against this."

	<znOption>

	^ 16 * 1024 * 1024
]

{ #category : #options }
ZnOptions class >> maximumNumberOfConcurrentConnections [
	"Set the maximum number of concurrent connections that I will accept.
	When this threshold is reached, a 503 Service Unavailable response will be sent
	and the connection will be closed. This protects me from certain forms of attacks.
	It is possible to raise this number when other system parameters are adjusted as well."

	<znOption>

	^ 32
]

{ #category : #options }
ZnOptions class >> myOption [
	"This is an example option.
	This class side method defines the option and its global default value"

	<znOption>

	^ 'myOptionDefaultValue'
]

{ #category : #'instance creation' }
ZnOptions class >> onClass: targetClass [
	| dictionary |
	dictionary := Dictionary new.
	(Pragma allNamed: #znOption in: targetClass class)
		do: [ :each |
			dictionary
				at: each method selector
				put: (each method methodClass instanceSide perform: each method selector) ].
	^ self new
		options: dictionary;
		writable: false;
		yourself
]

{ #category : #options }
ZnOptions class >> queryKeyValueSafeSet [
	"When encoding the key/value pairs of a URL or form-url-encoded entity,
	the characters part of this safe set are not percent encoded.
	Some servers expect more characters to be percent encoded, some less,
	in which case this option can be modified."

	<znOption>

	^ 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.~!$''()*,;:@/?'

	"When a query is interpreted as a list of key=value&.. pairs,
	it is better to encode = and & and leave them out of the safe set.
	Furthermore, since + is interpreted as space in the query part,
	it is unsafe as well. This is a restriction of #querySafeSet"
]

{ #category : #'class initialization' }
ZnOptions class >> resetGlobalDefault [
	globalDefault := nil
]

{ #category : #private }
ZnOptions class >> root [
	^ self onClass: self
]

{ #category : #options }
ZnOptions class >> serverString [
	"The string a server uses in the Server header of responses to identify itself.
	To provide malicious parties as little information as possible it could make sense to overwrite this.
	Also, to idenitify yourself as a specific server, you could use its server string."

	<znOption>

	^ ZnConstants defaultServerString
]

{ #category : #options }
ZnOptions class >> signalProgress [
	"Boolean indicating if HTTPProgress notifications are signalled during the transfer of HTTP resources.
	Although unhandled notifications are harmless, it is a bit more efficient not to signal them."

	<znOption>

	^ false
]

{ #category : #options }
ZnOptions class >> userAgentString [
	"The value a client uses in the User-Agent header of requests to identify itself.
	To provide as little information to a server it could make sense to overwrite this.
	Also to idenitify yourself as a specific client, you could use its user agent string."

	<znOption>

	^ ZnConstants defaultUserAgent
]

{ #category : #accessing }
ZnOptions >> allBindings [
	| bindings |
	bindings := Dictionary new.
	parent ifNotNil: [
		parent allBindings keysAndValuesDo: [ :key :value |
			bindings at: key put: value ] ].
	options ifNotNil: [
		options keysAndValuesDo: [ :key :value |
			bindings at: key put: value ] ].
	^ bindings
]

{ #category : #accessing }
ZnOptions >> at: key [
	options ifNotNil: [ options at: key ifPresent: [ :value | ^ value ] ].
	^ parent
		ifNil: [ KeyNotFound signalFor: key in: self ]
		ifNotNil: [ parent at: key ]
]

{ #category : #accessing }
ZnOptions >> at: key put: value [
	writable
		ifFalse: [ (ModificationForbidden
				for: self
				at: key
				with: value
				retrySelector: #at:put:) signal ].
	(parent notNil and: [ parent includesKey: key ])
		ifFalse: [ KeyNotFound signalFor: key in: self ].
	options ifNil: [ options := Dictionary new ].
	^ options at: key put: value
]

{ #category : #copying }
ZnOptions >> clone [
	^ (self class new)
		parent: self;
		writable: true;
		yourself
]

{ #category : #execution }
ZnOptions >> conditionallyDuring: block [
	^ ZnCurrentOptions value
		ifNil: [ ZnCurrentOptions value: self during: block ]
		ifNotNil: block
]

{ #category : #execution }
ZnOptions >> during: block [
	^ ZnCurrentOptions value: self during: block
]

{ #category : #testing }
ZnOptions >> includesKey: key [
	^ (options notNil and: [ options includesKey: key ])
		or: [ parent notNil and: [ parent includesKey: key ] ]
]

{ #category : #private }
ZnOptions >> options: dictionary [
	options
		ifNotNil: [ (ModificationForbidden
				for: self
				at: nil
				with: dictionary
				retrySelector: #options:) signal ].
	options := dictionary
]

{ #category : #private }
ZnOptions >> parent: otherOptions [
	parent
		ifNotNil: [ (ModificationForbidden
				for: self
				at: nil
				with: otherOptions
				retrySelector: #parent:) signal ].
	parent := otherOptions
]

{ #category : #initialization }
ZnOptions >> reset [
	options ifNotNil: [ options removeAll ]
]

{ #category : #private }
ZnOptions >> writable: boolean [
	writable
		ifNotNil: [ (ModificationForbidden
				for: self
				at: nil
				with: boolean
				retrySelector: #writable:) signal ].
	writable := boolean
]
