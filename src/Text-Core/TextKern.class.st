"
A TextKern encodes a kerning change applicable over a given range of text.  Positive values of kern spread letters out, negative kern will cause them to overlap more.  Note that kerns other than 0 will display somewhat slower, as kerning is not yet supported in the text scanning primitive. 

-----exemple-------
| stream |
stream := TextStream on: (Text new: 100).
stream 
	withAttribute: (TextKern  kern: 1) do: [ stream nextPutAll: 'Pharo is cool'];
	cr;
	nextPutAll: 'Pharo is cool'; cr;
	withAttribute: (TextKern kern: -1) do: [stream nextPutAll: 'Pharo is cool'].
TextMorph new 
	newContents: stream contents;
	openInWindow.
"
Class {
	#name : #TextKern,
	#superclass : #TextAttribute,
	#instVars : [
		'kern',
		'active'
	],
	#category : #'Text-Core-Attributes'
}

{ #category : #'instance creation' }
TextKern class >> kern: kernValue [
	^ self new kern: kernValue
]

{ #category : #comparing }
TextKern >> = other [ 
	^ (other class == self class) 
		and: [other kern = kern]
]

{ #category : #accessing }
TextKern >> dominatedByCmd0 [
	"Cmd-0 should turn off kerning"
	^ true
]

{ #category : #accessing }
TextKern >> dominates: other [
	"NOTE: The use of active in this code is specific to its use in the method
		Text class addAttribute: att toArray: others"
	(active and: [other class == self class and: [other kern + kern = 0]])
		ifTrue: [active := false.  ^ true].  "can only dominate once"
	^ false
]

{ #category : #comparing }
TextKern >> hash [
	"#hash is re-implemented because #= is re-implemented"
	^kern hash
]

{ #category : #testing }
TextKern >> isKern [
	^true
]

{ #category : #accessing }
TextKern >> kern [
	^ kern
]

{ #category : #accessing }
TextKern >> kern: kernValue [
	kern := kernValue.
	self reset.
]

{ #category : #initialization }
TextKern >> reset [
	active := true
]

{ #category : #accessing }
TextKern >> set [
	^ active
]
