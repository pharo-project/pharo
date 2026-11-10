"
I am a style element of the custom style.

I am just a proxy object for one of the groups stored in `SHCustomStyleSettings`
"
Class {
	#name : 'SHStyleElement',
	#superclass : 'Object',
	#instVars : [
		'group',
		'color',
		'emphasis'
	],
	#category : 'Shout-Styling',
	#package : 'Shout',
	#tag : 'Styling'
}

{ #category : 'accessing' }
SHStyleElement >> color [

"	^ SHCustomStyle colorForGroup: group"
	^ color
	
]

{ #category : 'accessing' }
SHStyleElement >> color: aColor [

	color = aColor ifTrue: [ ^ self ].
	color := aColor.
	self triggerEvent: #changed with: self
]

{ #category : 'accessing' }
SHStyleElement >> emphasis [

"	^ SHCustomStyle emphasisForGroup: group"
	^ emphasis
	
]

{ #category : 'accessing' }
SHStyleElement >> emphasis: anEmphasis [

	emphasis = anEmphasis ifTrue: [ ^ self ].
	emphasis := anEmphasis.
	self triggerEvent: #changed with: self.
	
]

{ #category : 'accessing' }
SHStyleElement >> group [

	^ group
]

{ #category : 'accessing' }
SHStyleElement >> group: aString [ 
	group := aString
]

{ #category : 'printing' }
SHStyleElement >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll: ' group: '.
	group printOn: aStream.
	aStream nextPutAll: ' (color: '.
	"aStream nextPutAll: self color shortPrintString."
	aStream nextPut: $#.
	aStream nextPutAll: self color asHexString.
	aStream nextPutAll: ', emphasis: '.
	self emphasis printOn: aStream.
	aStream nextPutAll: ')'.
]
