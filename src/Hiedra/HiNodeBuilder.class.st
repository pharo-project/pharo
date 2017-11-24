"
I know how to build ruler nodes.
"
Class {
	#name : #HiNodeBuilder,
	#superclass : #HiBuilder,
	#instVars : [
		'color',
		'width'
	],
	#category : #'Hiedra-Builders'
}

{ #category : #accessing }
HiNodeBuilder >> color [
	
	^ color
]

{ #category : #accessing }
HiNodeBuilder >> color: anObject [
	
	color := anObject
]

{ #category : #initialization }
HiNodeBuilder >> initialize [

	super initialize.

	width := 4.7.
	color := Smalltalk ui theme textColor.

]

{ #category : #building }
HiNodeBuilder >> renderer [
	^ HiNodeRenderer new
		nodeBuilder: self;
		yourself
]

{ #category : #accessing }
HiNodeBuilder >> width [
	^ width
]

{ #category : #accessing }
HiNodeBuilder >> width: anObject [
	
	width := anObject
]
