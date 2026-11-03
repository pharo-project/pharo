"
I am a style element of the custom style.

I am just a proxy object for one of the groups stored in `SHCustomStyleSettings`
"
Class {
	#name : 'SHStyleElement',
	#superclass : 'Object',
	#instVars : [
		'group'
	],
	#category : 'Shout-Styling',
	#package : 'Shout',
	#tag : 'Styling'
}

{ #category : 'accessing' }
SHStyleElement class >> group: groupName [

	^ self basicNew
		  group: groupName;
		  yourself
]

{ #category : 'instance creation' }
SHStyleElement class >> new [
	"Please use #group: to create an instance"
	self shouldNotImplement
]

{ #category : 'settings' }
SHStyleElement class >> settingInputWidgetForNode: aSettingNode [

	| theme |
	theme := UITheme builder.
	^ theme newRow: {
			  (theme
				   newColorChooserFor: aSettingNode realValue
				   getColor: #color
				   setColor: #color:
				   help: 'Choose token color').
			  ((theme
				    newDropListFor: aSettingNode realValue
				    list: #( #bold #italic #normal #'bold italic' )
				    getSelected: #emphasis
				    setSelected: #emphasis:
				    getEnabled: nil
				    useIndex: false
				    help: 'Choose token emphasis')
				   hResizing: #rigid;
				   width: 100) }
]

{ #category : 'accessing' }
SHStyleElement >> color [

	^ SHCustomStyle colorForGroup: group
]

{ #category : 'accessing' }
SHStyleElement >> color: aColor [

	^ SHCustomStyle color: aColor forGroup: group
]

{ #category : 'accessing' }
SHStyleElement >> emphasis [

	^ SHCustomStyle emphasisForGroup: group
]

{ #category : 'accessing' }
SHStyleElement >> emphasis: anEmphasis [

	^ SHCustomStyle emphasis: anEmphasis forGroup: group
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
