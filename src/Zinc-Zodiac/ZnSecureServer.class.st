"
I am ZnSecureServer, an implementation of an HTTPS server.
I am a ZnMultiThreadedServer

(ZnSecureServer on: 1443)
	certificate: '/home/sven/ssl/key-cert.pem';
	logToTranscript;
	start;
	yourself.
	
Disclaimer: this is an experimental proof of concept.
"
Class {
	#name : #ZnSecureServer,
	#superclass : #ZnMultiThreadedServer,
	#instVars : [
		'certificate'
	],
	#category : #'Zinc-Zodiac'
}

{ #category : #accessing }
ZnSecureServer >> certificate [
	^ certificate
]

{ #category : #accessing }
ZnSecureServer >> certificate: anObject [
	certificate := anObject
]

{ #category : #accessing }
ZnSecureServer >> scheme [
	^ #https
]

{ #category : #private }
ZnSecureServer >> socketStreamOn: socket [
	| stream |
	stream := ZdcSecureSocketStream on: socket.
	stream sslSession certificateName: self certificate.
	stream accept.
	^ stream
]
