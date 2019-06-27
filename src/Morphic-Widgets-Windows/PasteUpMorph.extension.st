Extension { #name : #PasteUpMorph }

{ #category : #'*Morphic-Widgets-Windows' }
PasteUpMorph >> fitAllVisibleWindows [
	"Fit all windows as visible"
	|allowedArea|
	allowedArea := RealEstateAgent maximumUsableAreaInWorld: self currentWorld.
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
PasteUpMorph >> modalLockTo: aSystemWindow [
	"Don't lock the world nor the aSystemWindow! Lock the submorphs."
	
	|lockStates|
	
	lockStates := IdentityDictionary new.
	"lock all submorphs"
	self submorphsDo: [:m |
		lockStates at: m put: m isLocked.
		m lock].
	"don't lock the given window"
	aSystemWindow unlock.
	lockStates at: aSystemWindow put: aSystemWindow isLocked.
	
	self
		setProperty: #submorphLockStates
		toValue: lockStates
]

{ #category : #'*Morphic-Widgets-Windows' }
PasteUpMorph >> modalUnlockFrom: aSystemWindow [
	"Don't unlock the world! Unlock the submorphs
	that were not originally locked."
	
	|lockStates|
	lockStates := self
		valueOfProperty: #submorphLockStates
		ifAbsent: [^self].
	self removeProperty: #submorphLockStates.
	lockStates keysAndValuesDo: [:m :locked |
		locked ifFalse: [m unlock]]
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
PasteUpMorph >> openModal: aSystemWindow [
	"Open the given window locking the receiver until it is dismissed.
	Set the pane color to match the current theme.
	Answer the system window."
	
	aSystemWindow setWindowColor: self theme windowColor.
	^ super openModal: aSystemWindow
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
