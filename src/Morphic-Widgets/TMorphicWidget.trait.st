"
I make a morph a widget: it is styled by a theme. Morphic's core morphs are plain graphical elements that nobody styles; widgets ask their theme for colors, sizes and the widgets they build.

My theme is the one set on me or on one of my owners (theme:), or else the current theme (UITheme current).
"
Trait {
	#name : 'TMorphicWidget',
	#category : 'Morphic-Widgets-Theme',
	#package : 'Morphic-Widgets',
	#tag : 'Theme'
}

{ #category : 'balloon help' }
TMorphicWidget >> balloonHelpDelayTime [

	^ self theme settings balloonHelpDelayTime
]

{ #category : 'balloon help' }
TMorphicWidget >> balloonHelpEnabled [

	^ self theme settings balloonHelpEnabled
]

{ #category : 'theme' }
TMorphicWidget >> basicTheme: aUITheme [
	"Set my theme without notification of change"

	self theme = aUITheme ifFalse: [ self setProperty: #theme toValue: aUITheme ]
]

{ #category : 'theme' }
TMorphicWidget >> defaultBackgroundColor [
	"Answer the color to be used as the base window color for a window whose model is an object of the receiver's class"

	^ self theme windowColorFor: self
]

{ #category : 'theme' }
TMorphicWidget >> defaultBorderWidth [
	"answer the default border width for the receiver"

	^ self theme borderWidth
]

{ #category : 'focus' }
TMorphicWidget >> focusIndicatorMorph [
	"Answer the focus indicator morph for the receiver."

	^ self theme focusIndicatorMorphFor: self
]

{ #category : 'balloon help' }
TMorphicWidget >> maxBalloonHelpLineLength [

	^ self theme settings maxBalloonHelpLineLength
]

{ #category : 'menus' }
TMorphicWidget >> menuKeyboardControl [

	^ self theme settings menuKeyboardControl
]

{ #category : 'balloon help' }
TMorphicWidget >> showBalloon: msgString hand: aHand [
	"Pop up a balloon containing the given string,
	first removing any existing BalloonMorphs in the world."

	| w h |
	(w := self world) ifNil: [ ^ self ].
	h := aHand ifNil: [ w activeHand ].
	(self theme builder newBalloonHelp: msgString for: self balloonHelpAligner) popUpFor: self hand: h
]

{ #category : 'focus' }
TMorphicWidget >> tabAmongFields [

	^ self theme settings tabAmongFields or: [ self hasProperty: #tabAmongFields ]
]

{ #category : 'theme' }
TMorphicWidget >> theme [
	"Answer my theme: the one set on me or on one of my owners, or else the current theme"

	self withAllOwnersDo: [ :morph | (morph valueOfProperty: #theme) ifNotNil: [ :aTheme | ^ aTheme ] ].
	^ UITheme current
]

{ #category : 'theme' }
TMorphicWidget >> theme: aUITheme [
	"Set my theme and propagate theme changed to my submorphs"

	self theme = aUITheme ifFalse: [
		self setProperty: #theme toValue: aUITheme.
		self themeChanged ]
]
