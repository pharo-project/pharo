"
Announce that a metalink changed
"
Class {
	#name : 'MetalinkChanged',
	#superclass : 'SystemAnnouncement',
	#instVars : [
		'link',
		'action',
		'node'
	],
	#category : 'Reflectivity-Core',
	#package : 'Reflectivity',
	#tag : 'Core'
}

{ #category : 'instance creation' }
MetalinkChanged class >> linkAdded: link [
	^ self new
		link: link;
		action: #add;
		yourself
]

{ #category : 'instance creation' }
MetalinkChanged class >> linkAdded: link toNode: aNode [
	^ (self linkAdded: link)
		node: aNode;
		yourself
]

{ #category : 'instance creation' }
MetalinkChanged class >> linkRemoved: link [
	^ self new
		link: link;
		action: #remove;
		yourself
]

{ #category : 'instance creation' }
MetalinkChanged class >> linkRemoved: link fromNode: aNode [
	^ (self linkRemoved: link)
		node: aNode;
		yourself
]

{ #category : 'accessing' }
MetalinkChanged >> action [
	^ action
]

{ #category : 'accessing' }
MetalinkChanged >> action: anObject [
	action := anObject
]

{ #category : 'testing' }
MetalinkChanged >> isAdd [
	^action = #add
]

{ #category : 'testing' }
MetalinkChanged >> isRemove [
	^action = #remove
]

{ #category : 'accessing' }
MetalinkChanged >> link [
	^ link
]

{ #category : 'accessing' }
MetalinkChanged >> link: anObject [
	link := anObject
]

{ #category : 'accessing' }
MetalinkChanged >> node [
	^ node
]

{ #category : 'accessing' }
MetalinkChanged >> node: anObject [
	node := anObject
]
