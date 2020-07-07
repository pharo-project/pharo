Class {
	#name : #MicAbstractInlineBlockWithUrl,
	#superclass : #MicAbstractInlineBlock,
	#instVars : [
		'url'
	],
	#category : #'Microdown-ModelInline'
}

{ #category : #obsolete }
MicAbstractInlineBlockWithUrl class >> from: aStartInteger to: anEndInteger withKind: aKind withSubstring: aString withChildren: aChildren withURL: aURL [
	^ (self from: aStartInteger to: anEndInteger withKind: aKind withSubstring: aString withChildren: aChildren) url: aURL; urlTransformation; yourself
]

{ #category : #accessing }
MicAbstractInlineBlockWithUrl >> url [
	^ url
]

{ #category : #accessing }
MicAbstractInlineBlockWithUrl >> url: anURL [
	url := anURL
]

{ #category : #transformation }
MicAbstractInlineBlockWithUrl >> urlTransformation [
	self url: (ZnUrl fromString: self url)
]
