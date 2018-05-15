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
	| cache |
	cache := self assertOriginResolves: #cache
]

{ #category : #tests }
PlatformResolverTest >> testHome [
	| home |
	home := self assertOriginResolves: #home.
	self assert: home isDirectory
]
