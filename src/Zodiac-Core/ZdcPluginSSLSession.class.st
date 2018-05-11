"
I am ZdcPluginSSLSession, an object managing the secure communication between two parties.

I am a ZdcAbstractSSLSession.

I am a wrapper for the SqueakSSL plugin.

I am probably too primitive to be used directly, see ZnSecureSocketStream for a higher level client.

Ackowledgement: based on the original SqueakSSL code.
"
Class {
	#name : #ZdcPluginSSLSession,
	#superclass : #ZdcAbstractSSLSession,
	#instVars : [
		'handle'
	],
	#category : #'Zodiac-Core'
}

{ #category : #operations }
ZdcPluginSSLSession >> accept: srcBuf from: start to: stop into: dstBuf [
	"Start or continue the server handshake using the given input token"

	^ self primitiveSSL: handle accept: srcBuf startingAt: start count: stop - start + 1 into: dstBuf
]

{ #category : #accessing }
ZdcPluginSSLSession >> certificateName [
	"The name of the local certificate to provide to the remote peer"

	^ self primitiveSSL: handle getStringProperty: 1
]

{ #category : #accessing }
ZdcPluginSSLSession >> certificateName: aString [
	"Sets the name of the local certificate to provide to the remote peer.
	OpenSSL: The name is the full path to a .pem file.
	WinSSL: The name is matched against the 'subject' of a certificate in the cert store"

	^ self primitiveSSL: handle setStringProperty: 1 toValue: (aString ifNil: [ '' ])
]

{ #category : #accessing }
ZdcPluginSSLSession >> certificateVerificationState [
	"Returns the certificate verification bits. The returned value indicates
	whether the certificate is valid. The two standard values are:
		0   - The certificate is valid
		-1  - No certificate has been provided by the peer
	Otherwise, the result is a bit mask of the following values:
		1   - If set, there is an unspecified issue with the cert (generic error)
		2   - If set, the root CA is untrusted (usually a self-signed cert)
		4   - If set, the certificate is expired
		8   - If set, the certificate is used for the wrong purpose
		16 - If set, the CN of the certificate is invalid
		32 - If set, the certificate was revoked"
		
	^ self primitiveSSL: handle getIntProperty: 3
]

{ #category : #operations }
ZdcPluginSSLSession >> connect: srcBuf from: start to: stop into: dstBuf [
	"Start or continue the client handshake using the given input token"

	^ self primitiveSSL: handle connect: srcBuf startingAt: start count: stop - start + 1 into: dstBuf
]

{ #category : #private }
ZdcPluginSSLSession >> constantsSQSSL_PROP_LOGLEVEL [

	^ 1
]

{ #category : #operations }
ZdcPluginSSLSession >> decrypt: srcBuf from: start to: stop into: dstBuf [
	"Decrypt the input in srcBuf from start to stop into dstBuf."

	^ self primitiveSSL: handle decrypt: srcBuf startingAt: start count: stop - start + 1 into: dstBuf
]

{ #category : #initialization }
ZdcPluginSSLSession >> destroy [
	"Destroys the platform handle in the VM plugin"

	handle isNil ifTrue: [ ^ self ].  
	self primitiveSSLDestroy: handle.
	handle := nil
]

{ #category : #initialization }
ZdcPluginSSLSession >> enableLogging [
	"Enable logging of the native code inside the plugin"
	
	self logging: true
]

{ #category : #operations }
ZdcPluginSSLSession >> encrypt: srcBuf from: start to: stop into: dstBuf [
	"Encrypt the input in srcBuf from start to stop into dstBuf."

	^ self primitiveSSL: handle encrypt: srcBuf startingAt: start count: stop - start + 1 into: dstBuf
]

{ #category : #initialization }
ZdcPluginSSLSession >> initialize [
	"Initialize the receiver"

	[ handle := self primitiveSSLCreate ]
		on: PrimitiveFailed 
		do: [ :exception |
			"Give a more human friendly error message"
			ZdcPluginMissing signal ].
	self logging: false

]

{ #category : #testing }
ZdcPluginSSLSession >> isConnected [
	"Returns true if the SSL handshake has been completed"

	^ self sslState = 3
]

{ #category : #accessing }
ZdcPluginSSLSession >> logging [

	^ ((self primitiveSSL: handle getIntProperty: self constantsSQSSL_PROP_LOGLEVEL) = 0) not
]

{ #category : #accessing }
ZdcPluginSSLSession >> logging: boolean [

	self primitiveSSL: handle setIntProperty: self constantsSQSSL_PROP_LOGLEVEL toValue: (boolean ifFalse: [ 0 ] ifTrue: [ 1 ]).

]

{ #category : #accessing }
ZdcPluginSSLSession >> peerCertificateName [
	"Returns the certificate name of the remote peer.
	The method only returns a name if the certificate has been verified"

	^ self primitiveSSL: handle getStringProperty: 0
]

{ #category : #accessing }
ZdcPluginSSLSession >> pluginVersion [
	"Returns the version of the plugin"

	^ self primitiveSSL: handle getIntProperty: 0
]

{ #category : #primitives }
ZdcPluginSSLSession >> primitiveSSL: sslHandle accept: srcbuf startingAt: start count: length into: dstbuf [
	"Primitive. Starts or continues a server handshake using the provided data.
	Will eventually produce output to be sent to the client.
	Returns:
		> 0   Number of bytes to be sent to the server
		= 0   Success. The connection is established
		= -1  More input is required
		< -1  Other errors"
		
	<primitive: 'primitiveAccept' module: 'SqueakSSL'>
	
	^ self primitiveFailed
]

{ #category : #primitives }
ZdcPluginSSLSession >> primitiveSSL: sslHandle connect: srcbuf startingAt: start count: length into: dstbuf [
	"Primitive. Starts or continues a client handshake using the provided data.
	Will eventually produce output to be sent to the server.
	Returns:
		> 0   Number of bytes to be sent to the server
		= 0   Success. The connection is established
		= -1  More input is required
		< -1  Other errors"
	
	<primitive: 'primitiveConnect' module: 'SqueakSSL'>
	
	^ self primitiveFailed
]

{ #category : #primitives }
ZdcPluginSSLSession >> primitiveSSL: sslHandle decrypt: srcbuf startingAt: start count: length into: dstbuf [
	"Primitive. Takes incoming data for decryption and continues to decrypt data.
	Returns the number of bytes produced in the output"

	<primitive: 'primitiveDecrypt' module: 'SqueakSSL'>
	
	^ self primitiveFailed
]

{ #category : #primitives }
ZdcPluginSSLSession >> primitiveSSL: sslHandle encrypt: srcbuf startingAt: start count: length into: dstbuf [
	"Primitive. Encrypts the incoming buffer into the result buffer.
	Returns the number of bytes produced as a result"

	<primitive: 'primitiveEncrypt' module: 'SqueakSSL'>
	
	^ self primitiveFailed
]

{ #category : #primitives }
ZdcPluginSSLSession >> primitiveSSL: sslHandle getIntProperty: propID [
	"Primitive. Returns a string property from an SSL session"

	<primitive: 'primitiveGetIntProperty' module: 'SqueakSSL'>
	
	^ self primitiveFailed
]

{ #category : #primitives }
ZdcPluginSSLSession >> primitiveSSL: sslHandle getStringProperty: propID [
	"Primitive. Returns a string property from an SSL session"

	<primitive: 'primitiveGetStringProperty' module: 'SqueakSSL'>
	
	^ self primitiveFailed
]

{ #category : #primitives }
ZdcPluginSSLSession >> primitiveSSL: sslHandle setIntProperty: propID toValue: anInteger [
	"Primitive. Sets an integer property in an SSL session"

	<primitive: 'primitiveSetIntProperty' module: 'SqueakSSL'>
	
	^ self primitiveFailed
]

{ #category : #primitives }
ZdcPluginSSLSession >> primitiveSSL: sslHandle setStringProperty: propID toValue: aString [
	"Primitive. Sets a string property in an SSL session"

	<primitive: 'primitiveSetStringProperty' module: 'SqueakSSL'>
	
	^ self primitiveFailed
]

{ #category : #primitives }
ZdcPluginSSLSession >> primitiveSSLCreate [
	"Primitive. Creates and returns a new SSL handle in the VM plugin"

	<primitive: 'primitiveCreate' module: 'SqueakSSL'>
	
	^ self primitiveFailed
]

{ #category : #primitives }
ZdcPluginSSLSession >> primitiveSSLDestroy: sslHandle [
	"Primitive. Destroys the SSL session handle in the VM plugin"

	<primitive: 'primitiveDestroy' module: 'SqueakSSL'>
	
	^ self primitiveFailed
]

{ #category : #accessing }
ZdcPluginSSLSession >> serverName [
	"Get the Server Name Indication (SNI) property"

	^ self primitiveSSL: handle getStringProperty: 2
]

{ #category : #accessing }
ZdcPluginSSLSession >> serverName: serverName [
	"Set the Server Name Indication (SNI) to serverName, 
	the fully qualified domain name of the host to connect to.
	This should be set by a client before #connect is attempted."
	
	^ self primitiveSSL: handle setStringProperty: 2 toValue: (serverName ifNil: [''])
]

{ #category : #accessing }
ZdcPluginSSLSession >> sslState [
	"Returns the current state of the SSL connection:
		0 - Unused
		1 - In accept handshake
		2 - In connect handshake
		3 - Connected"
		
	^ self primitiveSSL: handle getIntProperty: 2
		
]
