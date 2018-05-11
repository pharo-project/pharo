"
I resolve origins by consulting the user. I maintain a cache of the user's responses.
"
Class {
	#name : #InteractiveResolver,
	#superclass : #FileSystemResolver,
	#instVars : [
		'cache'
	],
	#category : #'FileSystem-Core-Base-Resolver'
}

{ #category : #accessing }
InteractiveResolver >> flushLocalCache [
	cache := IdentityDictionary new
]

{ #category : #initialization }
InteractiveResolver >> initialize [
	self flushLocalCache
]

{ #category : #resolving }
InteractiveResolver >> resolve: origin [
	^ cache at: origin ifAbsent: [self unknownOrigin: origin]
	

]

{ #category : #resolving }
InteractiveResolver >> unknownOrigin: origin [
	| reference |
	^ (next ifNotNil: [next resolve: origin]) ifNil: 
			[reference := ResolutionRequest for: origin.
			reference ifNotNil: [cache at: origin put: reference]]
]
