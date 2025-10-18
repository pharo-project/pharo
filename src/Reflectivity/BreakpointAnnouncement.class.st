"
Breakpoint announcements indicates that a breakpoint-related event occured.
I am an abstract class, I should be subclassed with concrete announcements representing breakpoint events of interests.  

Such events are:
- when a new breakpoint is added
- when a breakpoint is hit
- when a breakpoint is removed

Breakpoints announcements are announced by the system announcer.
To subscribe to a breakpoint announcement, do for example:
```Smalltalk
SystemAnnouncer uniqueInstance weak when: BreakpointAdded send: #buildTableData to: self
```
where you should replace `BreakpointAdded` by the announcement you are interested in, and `#buildTableData` by the message you want to send to your object (here it is sent to `self`).

When a client is notified of one of my announcements subclasses, the client can find:
- the metalink defining the break instrumentation using `#link`
- the nodes on which that breakpoint is installed unsing `#nodes` (the same breakpoint can be installed on many nodes, _e.g._, for breakpoints installed on all writings of a variable)
- the breakpoint object itself using `#breakpoint`

"
Class {
	#name : 'BreakpointAnnouncement',
	#superclass : 'SystemAnnouncement',
	#instVars : [
		'breakpoint',
		'nodes'
	],
	#category : 'Reflectivity-Breakpoints',
	#package : 'Reflectivity',
	#tag : 'Breakpoints'
}

{ #category : 'instance creation' }
BreakpointAnnouncement class >> on: aBreakpoint nodes: nodes [
	^ self new
		breakpoint: aBreakpoint;
		nodes: nodes;
		yourself
]

{ #category : 'accessing' }
BreakpointAnnouncement >> breakpoint [
	^ breakpoint
]

{ #category : 'accessing' }
BreakpointAnnouncement >> breakpoint: anObject [
	breakpoint := anObject
]

{ #category : 'accessing' }
BreakpointAnnouncement >> link [
	^ self breakpoint link
]

{ #category : 'accessing' }
BreakpointAnnouncement >> nodes [
	^ nodes
]

{ #category : 'accessing' }
BreakpointAnnouncement >> nodes: anObject [
	nodes := anObject
]
