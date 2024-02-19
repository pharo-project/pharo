"
I hold preferences for the BeautifulComments (in class variables and methods on my class side).

## Settings
- In particular one can disable rich text rendering (showing the raw comments of the class), which is done in the settings browser `rendering`, or through my class side method `render: aBoolean`.

- As per fall of 2021 microdown and microdown support in Pharo is still in active development. Errors can occour! It is possible to set the comment to be robust to errors in the microdown in class comments. This is controlled by the `capture errors`, or through the class side method: 'captureErrors: aBoolean`.
"
Class {
	#name : 'BeautifulComments',
	#superclass : 'Object',
	#classVars : [
		'CaptureErrors',
		'Rendering'
	],
	#category : 'BeautifulComments-Core',
	#package : 'BeautifulComments',
	#tag : 'Core'
}

{ #category : 'settings' }
BeautifulComments class >> beautifulCommentsSettingsOn: aBuilder [
	"These settings are added to the settings defined in Microdown>>preferencesSettingsOn:"
	<systemsettings>
	(aBuilder setting: #rendering)
		parent: #microdownAndcomments;
		label: 'Enable richtext comments';
		default: true;
		target: self;
		description: self renderingDocForSetting.
	(aBuilder setting: #captureErrors)
		parent: #microdownAndcomments;
		label: 'Enable rendering error capture';
		default: true;
		target: self;
		description: self captureErrorsDocForSetting
]

{ #category : 'settings' }
BeautifulComments class >> captureErrors [
	^ CaptureErrors ifNil: [ CaptureErrors := true ]
]

{ #category : 'settings' }
BeautifulComments class >> captureErrors: aBoolean [ 
	CaptureErrors := aBoolean
]

{ #category : 'settings' }
BeautifulComments class >> captureErrorsDocForSetting [

	^ 'By default microdown errors in comment text will not cause debugger to open. Turn this of to get debugger to open on errors'
]

{ #category : 'rendering' }
BeautifulComments class >> render: aString [ 
	"Render a string, in case of error, just return it."
	|composer|
	composer := self richTextComposer.
	self rendering 	
		ifFalse: [ ^ aString ].
	self captureErrors 
		ifFalse: [ ^ composer asRichText: aString ].
	^ [ composer asRichText: aString ] 
			on: Error
			do: [ aString  ]
]

{ #category : 'rendering' }
BeautifulComments class >> renderComment: aString of: aClassOrPackage [ 
	"Return aString as part of the templated class comment, when rendering is on.
	Else aString."
	
	| builder |
	builder := Microdown builder.
	aClassOrPackage buildMicroDownUsing: builder withComment: aString.
	^ self render: builder contents
]

{ #category : 'settings' }
BeautifulComments class >> rendering [
	^ Rendering ifNil: [ true ]
]

{ #category : 'rendering' }
BeautifulComments class >> rendering: aBoolean [
	Rendering := aBoolean
]

{ #category : 'settings' }
BeautifulComments class >> renderingDocForSetting [

	^ 'Comments are by default rendered via microdown. When this setting is on, they are rendered as plain string (showing their Microdown definitions).'
]

{ #category : 'rendering' }
BeautifulComments class >> richTextComposer [
	^ MicRichTextComposer new
		textStyler: BeautifulCommentsTextStyler new;
		yourself.
]
