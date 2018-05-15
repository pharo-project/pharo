"
Signals that a connection to the specified host and port was refused.

	host		host which refused the connection
	port		prot to which the connection was refused

"
Class {
	#name : #ConnectionRefused,
	#superclass : #NetworkError,
	#instVars : [
		'host',
		'port'
	],
	#category : #'Network-Kernel-Exceptions'
}

{ #category : #'instance creation' }
ConnectionRefused class >> host: addressOrHostName port: portNumber [
	^ self new host: addressOrHostName port: portNumber
]

{ #category : #accessing }
ConnectionRefused >> host [
	^ host
]

{ #category : #accessing }
ConnectionRefused >> host: addressOrHostName port: portNumber [
	host := addressOrHostName.
	port := portNumber
]

{ #category : #accessing }
ConnectionRefused >> port [
	^ port
]
