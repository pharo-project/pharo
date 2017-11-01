Class {
	#name : #DockingBarMenuMorph,
	#superclass : #MenuMorph,
	#instVars : [
		'activatorDockingBar'
	],
	#category : 'Morphic-Widgets-Extra'
}

{ #category : #accessing }
DockingBarMenuMorph >> activatedFromDockingBar: aDockingBar [ 
	activatorDockingBar := aDockingBar
]

{ #category : #control }
DockingBarMenuMorph >> activeSubmenu: aSubmenu [ 
	super activeSubmenu: aSubmenu.
	aSubmenu
		ifNotNil: [activeSubMenu activatedFromDockingBar: nil]
]

{ #category : #rounding }
DockingBarMenuMorph >> roundedCorners [
	"Return a list of those corners to round"
	self wasActivatedFromDockingBar
		ifTrue: [
			activatorDockingBar isFloating
				ifTrue: [^ #(2 3 )].
			activatorDockingBar isAdheringToTop
				ifTrue: [^ #(2 3 )].
			activatorDockingBar isAdheringToBottom
				ifTrue: [^ #(1 4 )].
			activatorDockingBar isAdheringToLeft
				ifTrue: [^ #(3 4 )].
			activatorDockingBar isAdheringToRight
				ifTrue: [^ #(1 2 )]].
	^ super roundedCorners
]

{ #category : #copying }
DockingBarMenuMorph >> veryDeepInner: deepCopier [
	"Copy all of my instance variables.  Some need to be not copied at all, but shared.  	Warning!!  Every instance variable defined in this class must be handled.  We must also implement veryDeepFixupWith:.  See DeepCopier class comment."

	super veryDeepInner: deepCopier.
	activatorDockingBar := activatorDockingBar.  "Weakly copied"


]

{ #category : #accessing }
DockingBarMenuMorph >> wasActivatedFromDockingBar [
	"answer true if the receiver was activated from a docking bar"
	^ activatorDockingBar notNil
]
