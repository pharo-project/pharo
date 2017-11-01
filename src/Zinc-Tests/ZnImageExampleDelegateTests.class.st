Class {
	#name : #ZnImageExampleDelegateTests,
	#superclass : #TestCase,
	#category : 'Zinc-Tests'
}

{ #category : #private }
ZnImageExampleDelegateTests >> image [
	^ ZnClient new
		beOneShot;
		get: 'http://zn.stfx.eu/zn/Hot-Air-Balloon.gif';
		entity
]

{ #category : #testing }
ZnImageExampleDelegateTests >> testDefaultImage [
	self withServerDo: [ :server |
		| client |
		client := ZnClient new.
		client url: server localUrl; addPath: #image; queryAt: #raw put: #true.
		client get.
		self assert: client isSuccess.
		self assert: (client entity contentType matches: 'image/*' asZnMimeType).
		self assert: client entity equals: server delegate image.
		client close ]
]

{ #category : #testing }
ZnImageExampleDelegateTests >> testMainPage [
	self withServerDo: [ :server |
		| client |
		client := ZnClient new.
		client url: server localUrl; addPath: #image.
		client get.
		self assert: client isSuccess.
		self assert: (client entity contentType matches: ZnMimeType textHtml).
		self assert: (client contents includesSubstring: 'Image').
		client close ]
]

{ #category : #testing }
ZnImageExampleDelegateTests >> testUpload [
	self withServerDo: [ :server |
		| image client |
		image := self image.
		client := ZnClient new.
		client url: server localUrl; addPath: #image.
		client addPart: (ZnMimePart fieldName: #file entity: image).
		client post.
		self assert: client isSuccess.
		client resetEntity; queryAt: #raw put: #true.
		client get.
		self assert: client isSuccess.
		self assert: client entity equals: image.
		client close ]
]

{ #category : #private }
ZnImageExampleDelegateTests >> withServerDo: block [
	| server |
	server := ZnServer on: 1700 + 32 atRandom.
	[ 
		server start.
		self 
			assert: server isRunning & server isListening
			description: ('Failed to start server on port {1}. Is there one already?' format: { server port }).
		server delegate: ZnImageExampleDelegate new.
		block cull: server
	] 
		ensure: [ server stop ]
]
