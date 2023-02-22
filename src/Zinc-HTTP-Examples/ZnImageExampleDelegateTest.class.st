"
Functional unit tests for ZnImageExample
"
Class {
	#name : #ZnImageExampleDelegateTest,
	#superclass : #TestCase,
	#category : #'Zinc-HTTP-Examples'
}

{ #category : #private }
ZnImageExampleDelegateTest >> image [
	^ ZnClient new
		beOneShot;
		get: 'http://zn.stfx.eu/zn/Hot-Air-Balloon.gif';
		entity
]

{ #category : #testing }
ZnImageExampleDelegateTest >> testDefaultImage [
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
ZnImageExampleDelegateTest >> testMainPage [
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
ZnImageExampleDelegateTest >> testUpload [
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
ZnImageExampleDelegateTest >> withServerDo: block [
	ZnServer withOSAssignedPortDo: [ :server |
		server delegate: ZnImageExampleDelegate new.
		block cull: server ]
]
