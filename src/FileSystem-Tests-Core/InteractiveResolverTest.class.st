"
SUnit tests for InteractiveResolver
"
Class {
	#name : #InteractiveResolverTest,
	#superclass : #FileSystemResolverTest,
	#category : #'FileSystem-Tests-Core-Resolver'
}

{ #category : #running }
InteractiveResolverTest >> createResolver [
	^ InteractiveResolver new
]

{ #category : #accessing }
InteractiveResolverTest >> home [
	^ FileLocator imageDirectory resolve
]

{ #category : #tests }
InteractiveResolverTest >> testCached [
	[ resolver resolve: #home ]
		on: ResolutionRequest
		do: [ :req | req resume: self home ].
	self assertOriginResolves: #home
]

{ #category : #tests }
InteractiveResolverTest >> testNew [
	[self assertOriginResolves: #home]
		on: ResolutionRequest
		do: [:req | req resume: self home].
	
]
