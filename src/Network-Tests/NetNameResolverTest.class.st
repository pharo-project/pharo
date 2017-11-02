"
SUnit tests for NetNameResolver 
"
Class {
	#name : #NetNameResolverTest,
	#superclass : #TestCase,
	#category : 'Network-Tests-Kernel'
}

{ #category : #accessing }
NetNameResolverTest >> localHostAddressIsLoopBackAddress [
		^NetNameResolver localHostAddress = NetNameResolver loopBackAddress
]

{ #category : #accessing }
NetNameResolverTest >> localHostNameIsLoopBackName [
		^NetNameResolver localHostName = NetNameResolver loopBackName
]

{ #category : #testing }
NetNameResolverTest >> testLocalHostAddress [
	NetNameResolver isConnected
		ifTrue: [self deny: self localHostAddressIsLoopBackAddress]
		ifFalse: [self assert: self localHostAddressIsLoopBackAddress]
]

{ #category : #testing }
NetNameResolverTest >> testLocalHostName [
	self deny: NetNameResolver localHostName isNil.
	NetNameResolver isConnected
		ifTrue: [self deny: self localHostNameIsLoopBackName]
		ifFalse: [self assert: self localHostNameIsLoopBackName]
]
