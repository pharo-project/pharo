"
I am a style element of the custom style.

I am just a proxy object for one of the groups stored in `SHCustomStyleSettings`
"
Class {
	#name : 'SHCustomStyleElement',
	#superclass : 'Object',
	#instVars : [
		'group'
	],
	#category : 'Shout-Styling',
	#package : 'Shout',
	#tag : 'Styling'
}

{ #category : 'accessing' }
SHCustomStyleElement class >> group: groupName [

	^ self basicNew
		  group: groupName;
		  yourself
]

{ #category : 'instance creation' }
SHCustomStyleElement class >> new [
	"Please use #group: to create an instance"
	self shouldNotImplement
]

{ #category : 'settings' }
SHCustomStyleElement class >> settingInputWidgetForNode: aSettingNode [

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
SHCustomStyleElement >> color [

	^ SHCustomStyle colorForGroup: group
]

{ #category : 'accessing' }
SHCustomStyleElement >> color: aColor [

	^ SHCustomStyle color: aColor forGroup: group
]

{ #category : 'accessing' }
SHCustomStyleElement >> emphasis [

	^ SHCustomStyle emphasisForGroup: group
]

{ #category : 'accessing' }
SHCustomStyleElement >> emphasis: anEmphasis [

	^ SHCustomStyle emphasis: anEmphasis forGroup: group
]

{ #category : 'accessing' }
SHCustomStyleElement >> group: aString [ 
	group := aString
]
