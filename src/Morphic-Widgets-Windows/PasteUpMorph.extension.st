Extension { #name : #PasteUpMorph }

{ #category : #'*Morphic-Widgets-Windows' }
PasteUpMorph >> fitAllVisibleWindows [
	"Fit all windows as visible"
	|allowedArea|
	allowedArea := RealEstateAgent maximumUsableAreaInWorld: self world.
	(self  windowsSatisfying: [:w | w isCollapsed not])
		reverseDo:[:w | 
			w extent: w initialExtent.  
			w bounds:((w position extent: w initialExtent) translatedAndSquishedToBeWithin: allowedArea).
		].
	self displayWorld
]

{ #category : #'*Morphic-Widgets-Windows' }
PasteUpMorph >> isWindowActive: aSystemWindow [
	"Answer whether the given window is active.
	True if the current top window."
	
	^self currentWindow == aSystemWindow
]

{ #category : #'*Morphic-Widgets-Windows' }
PasteUpMorph >> modelWakeUpIn: aWindow [
	"I am the model of a SystemWindow, that has just been activated"

	self viewBox = aWindow panelRect ifFalse: [self viewBox: aWindow panelRect]
]

{ #category : #'*Morphic-Widgets-Windows' }
PasteUpMorph >> navigateVisibleWindowForward [
	"Change the active window to the next visible and not collapsed window."
 
	self nextVisibleWindow
		ifNil: [SystemWindow passivateTopWindow]
		ifNotNil: [:m | m activate]
 
]

{ #category : #'*Morphic-Widgets-Windows' }
PasteUpMorph >> navigateWindowBackward [
	"Change the active window to the previous window."

	self previousWindow ifNotNil: [:m |
		m isCollapsed ifTrue: [m collapseOrExpand].
		m activate]
]

{ #category : #'*Morphic-Widgets-Windows' }
PasteUpMorph >> navigateWindowForward [
	"Change the active window to the next window."

	self nextWindow ifNotNil: [:m |
		self currentWindow ifNotNil: [:w | w sendToBack].
		m isCollapsed ifTrue: [m collapseOrExpand].
		m activate]
]

{ #category : #'*Morphic-Widgets-Windows' }
PasteUpMorph >> nextVisibleWindow [
	"Answer the next (visible) window to navigate to."

	^self visibleSystemWindows 
		ifEmpty: [ nil ] 
		ifNotEmpty: [ :windows | 
			windows after: self currentWindow ifAbsent: [windows first]
		]

]

{ #category : #'*Morphic-Widgets-Windows' }
PasteUpMorph >> nextWindow [
	"Answer the next window to navigate to."

	|sys|
	sys := self systemWindows.
	sys ifEmpty: [^nil].
	^sys after: self currentWindow ifAbsent: [sys first]
]

{ #category : #'*Morphic-Widgets-Windows' }
PasteUpMorph >> previousWindow [
	"Answer the previous window to navigate to."

	|sys|
	sys := self systemWindows.
	sys ifEmpty: [^nil].
	^sys before: self currentWindow ifAbsent: [sys last]
]

{ #category : #'*Morphic-Widgets-Windows' }
PasteUpMorph >> systemWindows [
	"Answer the system windows in the world."
	
	^self submorphsSatisfying: [:m | m isSystemWindow]
]

{ #category : #'*Morphic-Widgets-Windows' }
PasteUpMorph >> visibleSystemWindows [
	"Answer the visible system windows in the world."
	
	^self submorphsSatisfying: [:m | m isSystemWindow and: [m visible]]
]
