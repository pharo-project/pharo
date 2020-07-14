"
My subclasses manage figures and links.

```
![Pharologo](https://files.pharo.org/media/logo/logo.png
```

![Pharologo](https://files.pharo.org/media/logo/logo.png)
"
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

{ #category : #obsolete }
MicAbstractInlineBlockWithUrl class >> from: aStartInteger to: anEndInteger withKind: aKind withSubstring: aString withURL: aURL [
	^ (self from: aStartInteger to: anEndInteger withKind: aKind withSubstring: aString withChildren: Array empty withURL: aURL)
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
