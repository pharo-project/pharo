"
A Shout style element
"
Class {
	#name : #SHStyleElement,
	#superclass : #Object,
	#instVars : [
		'tokens',
		'color',
		'emphasis'
	],
	#category : 'Shout-Styling'
}

{ #category : #settings }
SHStyleElement class >> settingInputWidgetForNode: aSettingNode [ 
	| theme |
	theme := UITheme builder.
	^ theme newRow: {theme
			newColorChooserFor: aSettingNode realValue
			getColor: #color
			setColor: #color:
			help: 'Choose token color'. (theme
			newDropListFor: aSettingNode realValue
			list: #(#bold #italic #normal 'bold italic' )
			getSelected: #emphasis
			setSelected: #emphasis:
			getEnabled: nil
			useIndex: false
			help: 'Choose token emphasis') hResizing: #rigid;
			 width: 100}
]

{ #category : #instance-creation }
SHStyleElement class >> withTokens: aCollection [ 
	^ self new tokens: aCollection;
		 yourself
]

{ #category : #accessing }
SHStyleElement >> color [
	^ color
		ifNil: [color := Color colorFrom: (SHPreferences customStyleTable
							detect: [:e | self tokens first = e first]) second]
]

{ #category : #accessing }
SHStyleElement >> color: anObject [ 
	color := anObject.
	SHPreferences applyStyle
]

{ #category : #accessing }
SHStyleElement >> emphasis [
	| style |
	^ emphasis
		ifNil: [style := SHPreferences customStyleTable
						detect: [:e | self tokens first = e first].
			style size > 2
				ifTrue: [emphasis := style third.
					emphasis isSymbol
						ifFalse: [emphasis := ' ' join: emphasis].
					emphasis]
				ifFalse: [#normal]]
]

{ #category : #accessing }
SHStyleElement >> emphasis: anObject [ 
	emphasis := anObject.
	SHPreferences applyStyle
]

{ #category : #styling }
SHStyleElement >> emphasisAsArray [
	^ self emphasis = 'bold italic'
		ifTrue: [#(#bold #italic )]
		ifFalse: [self emphasis]
]

{ #category : #printing }
SHStyleElement >> printOn: aStream [
	super printOn: aStream.
	color isNil
		ifFalse: [ 
			aStream
				nextPutAll: ' color: ';
				nextPutAll: color printString ].
	emphasis isNil
		ifFalse: [ 
			aStream
				nextPutAll: ' emphasis: ';
				nextPutAll: emphasis printString ].
	tokens isNil
		ifFalse: [ 
			aStream
				nextPutAll: ' tokens: ';
				nextPutAll: tokens printString ]
]

{ #category : #styling }
SHStyleElement >> styleForTable [
	^ self tokens
		collect: [:token | 
			| style |
			style := OrderedCollection with: token.
			style add: self color.
			self emphasis
				ifNotNil: [style add: self emphasisAsArray].
			style asArray]
]

{ #category : #accessing }
SHStyleElement >> tokens [
	^ tokens
]

{ #category : #accessing }
SHStyleElement >> tokens: anObject [ 
	tokens := anObject
]
