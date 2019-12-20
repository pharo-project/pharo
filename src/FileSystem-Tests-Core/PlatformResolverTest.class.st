"
SUnit tests for PlatformResolver
"
Class {
	#name : #PlatformResolverTest,
	#superclass : #FileSystemResolverTest,
	#category : #'FileSystem-Tests-Core-Resolver'
}

{ #category : #running }
PlatformResolverTest >> createResolver [
	^ PlatformResolver forCurrentPlatform
]

{ #category : #tests }
PlatformResolverTest >> testCache [
	self assertOriginResolves: #cache
]

{ #category : #tests }
PlatformResolverTest >> testCanAddOrigin [
	[ resolver class
		compile:
			'newOriginForTest
	<origin>
	^ self home / ''toto'''.
	self assert: (resolver supportedOrigins includes: #newOriginForTest) ]
		ensure: [ resolver class compiledMethodAt: #newOriginForTest ifPresent: [ :m | m removeFromSystem ] ]
]

{ #category : #tests }
PlatformResolverTest >> testHome [
	| home |
	home := self assertOriginResolves: #home.
	self assert: home isDirectory
]
