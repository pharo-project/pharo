Extension { #name : 'Breakpoint' }

{ #category : '*Reflectivity-Tests' }
Breakpoint class >> registerObserver: anObject [
	"Do not use this method, it is there for compatibility with old users.
	Instead, register your object yourself to 'SystemAnnouncer uniqueInstance' like below, but only for the announcements you need"

	SystemAnnouncer uniqueInstance
		when: BreakpointAdded send: #update: to: anObject;
		when: BreakpointHit send: #update: to: anObject;
		when: BreakpointRemoved send: #update: to: anObject
]

{ #category : '*Reflectivity-Tests' }
Breakpoint class >> unregisterObserver: anObject [
	"Do not use this method, it is there for compatibility with old users.
	Instead, unsubscribe yourself from 'SystemAnnouncer uniqueInstance' directly (like shown below)"

	SystemAnnouncer uniqueInstance unsubscribe: anObject
]
