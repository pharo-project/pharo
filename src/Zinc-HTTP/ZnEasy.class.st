"
I am ZnEasy, a simplified HTTP client for occasional use.

My class side implements a number of convenience methods to do common HTTP client operations.

	ZnEasy get: 'http://zn.stfx.eu/zn/small.html'.

For most requests, I return a response object. 

For a couple of message, I return an image Form.

	ZnEasy getPng: 'http://pharo.org/files/pharo.png'.

For my implementation I use ZnClient, a full featured HTTP client.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnEasy,
	#superclass : #Object,
	#category : #'Zinc-HTTP-Client-Server'
}

{ #category : #private }
ZnEasy class >> client [
	^ ZnClient new
		beOneShot;
		yourself
]

{ #category : #operations }
ZnEasy class >> delete: urlObject [
	^ self client
		url: urlObject;
		delete;
		response
]

{ #category : #operations }
ZnEasy class >> delete: urlObject username: username password: password [
	^ self client
		url: urlObject;
		setBasicAuthenticationUsername: username password: password;
		delete;
		response
	
]

{ #category : #operations }
ZnEasy class >> get: urlObject [
	"self get: 'http://stfx.eu/small.html'."
	
	^ self client
		url: urlObject;
		get;
		response
	
]

{ #category : #operations }
ZnEasy class >> get: urlObject username: username password: password [
	^ self client
		url: urlObject;
		setBasicAuthenticationUsername: username password: password;
		get;
		response
	
]

{ #category : #operations }
ZnEasy class >> getGif: urlObject [
	"self getGif: 'http://zn.stfx.eu/zn/Hot-Air-Balloon.gif'."
	
	^ self 
		getImageOfType: ZnMimeType imageGif 
		fromUrl: urlObject	
]

{ #category : #operations }
ZnEasy class >> getJpeg: urlObject [
	"self getJpeg: 'http://stfx.eu/sun-fire-x2100.jpg'."
	
	^ self 
		getImageOfType: ZnMimeType imageJpeg 
		fromUrl: urlObject
]

{ #category : #operations }
ZnEasy class >> getPng: urlObject [
	"self getPng: 'http://pharo.org/files/pharo.png'."
	"(self getPng: 'http://chart.googleapis.com/chart?cht=tx&chl=',
		'a^2+b^2=c^2' urlEncoded) asMorph openInHand."
	
	^ self 
		getImageOfType: ZnMimeType imagePng 
		fromUrl: urlObject
]

{ #category : #operations }
ZnEasy class >> head: urlObject [
	^ self client
		url: urlObject;
		head;
		response
]

{ #category : #operations }
ZnEasy class >> head: urlObject username: username password: password [
	^ self client
		url: urlObject;
		setBasicAuthenticationUsername: username password: password;
		head;
		response
]

{ #category : #operations }
ZnEasy class >> post: urlObject data: entity [
	^ self client
		url: urlObject;
		entity: entity;
		post;
		response
]

{ #category : #operations }
ZnEasy class >> post: urlObject data: entity username: username password: password [
	^ self client
		url: urlObject;
		setBasicAuthenticationUsername: username password: password;
		entity: entity;
		post;
		response
]

{ #category : #operations }
ZnEasy class >> put: urlObject data: entity [
	^ self client
		url: urlObject;
		entity: entity;
		put;
		response	
]

{ #category : #operations }
ZnEasy class >> put: urlObject data: entity username: username password: password [
	^ self client
		url: urlObject;
		setBasicAuthenticationUsername: username password: password;
		entity: entity;
		put;
		response	
]
