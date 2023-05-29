"
ZnClientFollowingRedirectEvent signals that a redirect to target is being followed. How many redirects to follow, if any is configurable.
"
Class {
	#name : #ZnClientFollowingRedirectEvent,
	#superclass : #ZnClientLogEvent,
	#instVars : [
		'target'
	],
	#category : #'Zinc-HTTP-Logging'
}

{ #category : #printing }
ZnClientFollowingRedirectEvent >> printContentsOn: stream [
	super printContentsOn: stream.
	stream << ' Following Redirect '; print: target
]

{ #category : #accessing }
ZnClientFollowingRedirectEvent >> target [
	^ target
]

{ #category : #accessing }
ZnClientFollowingRedirectEvent >> target: anObject [
	target := anObject
]
