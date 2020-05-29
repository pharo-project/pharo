"
I am a dummy breakpoint observer.
I am also an example observer.
I receives update notifications from the Breakpoint class whenever a breakpoint is added, modified or removed.
"
Class {
	#name : #DummyBreakpointObserver,
	#superclass : #Object,
	#instVars : [
		'tag'
	],
	#category : #'Reflectivity-Tools-Tests'
}

{ #category : #accessing }
DummyBreakpointObserver >> tag [
	^ tag
]

{ #category : #updating }
DummyBreakpointObserver >> update: anAspect [
	tag := anAspect 
]
