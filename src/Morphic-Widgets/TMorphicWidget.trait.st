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

{ #category : 'theme' }
TMorphicWidget >> basicTheme: aUITheme [
	"Set my theme without notification of change"

	self theme = aUITheme ifFalse: [ self setProperty: #theme toValue: aUITheme ]
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
