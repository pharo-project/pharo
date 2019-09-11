Extension { #name : #StrikeFont }

{ #category : #'*Text-Edition' }
StrikeFont class >> fromUser [
	"StrikeFont fromUser"
	^self fromUser: TextStyle defaultFont
]

{ #category : #'*Text-Edition' }
StrikeFont class >> fromUser: priorFont [
	
	^ UIManager default fontFromUser: priorFont.
]

{ #category : #'*Text-Edition' }
StrikeFont class >> fromUser: priorFont allowKeyboard: aBoolean [ 
	"Present a menu of available fonts, and if one is chosen, return it. Otherwise return nil."
	"StrikeFont fromUser"
	| fontList fontMenu spec font style |
	fontList := StrikeFont actualFamilyNames.
	fontMenu := UIManager default newMenuIn: self for: self.
	fontList do: 
		[ :fontName | | label active ptMenu | 
		style := TextStyle named: fontName.
		active := priorFont familyName sameAs: fontName.
		ptMenu := UIManager default newMenuIn: self for: self.
		style pointSizes do: 
			[ :pt | 	
			ptMenu 
				add: (active and: [ pt = priorFont pointSize ]) ->  (pt printString , ' pt')
				target: fontMenu
				selector: #modalSelection:
				argument: {  fontName . pt  } ].
		style isTTCStyle ifTrue: 
			[ ptMenu 
				add: 'new size'
				target: style
				selector: #addNewFontSizeDialog:
				argument: {  fontName. fontMenu  } ].
		label := active -> fontName.
		fontMenu 
			add: label
			subMenu: ptMenu ].
	spec := fontMenu 
		invokeModalAt: ActiveHand position
		in: self currentWorld
		allowKeyboard: aBoolean.
	spec ifNil: [ ^ nil ].
	style := TextStyle named: spec first.
	style ifNil: [ ^ self ].
	font := style fonts 
		detect: [ :any | any pointSize = spec last ]
		ifNone: [ nil ].
	^ font
]
