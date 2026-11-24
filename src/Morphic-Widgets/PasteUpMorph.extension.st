Extension { #name : 'PasteUpMorph' }

{ #category : '*Morphic-Widgets' }
PasteUpMorph >> closeAllWindowsDiscardingChanges [
	self world systemWindows do: [:w | w delete ]
]

{ #category : '*Morphic-Widgets' }
PasteUpMorph >> collapseAll [
	"Collapse all windows"
	(self windowsSatisfying: [:w | w isCollapsed not])
		reverseDo: [:w | w collapseOrExpand.  self displayWorld].
	self collapseNonWindows
]

{ #category : '*Morphic-Widgets' }
PasteUpMorph >> expandAll [
	"Expand all windows"
	(self  windowsSatisfying: [:w | w isCollapsed])
		reverseDo: [:w | w collapseOrExpand.  self displayWorld]
]

{ #category : '*Morphic-Widgets' }
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

{ #category : '*Morphic-Widgets' }
PasteUpMorph >> isWindowActive: aSystemWindow [
	"Answer whether the given window is active.
	True if the current top window."

	^self currentWindow == aSystemWindow
]

{ #category : '*Morphic-Widgets' }
PasteUpMorph >> navigateVisibleWindowForward [
	"Change the active window to the next visible and not collapsed window."

	self nextVisibleWindow
		ifNil: [SystemWindow passivateTopWindow]
		ifNotNil: [:m | m activate]
]

{ #category : '*Morphic-Widgets' }
PasteUpMorph >> navigateWindowBackward [
	"Change the active window to the previous window."

	self previousWindow ifNotNil: [:m |
		m isCollapsed ifTrue: [m collapseOrExpand].
		m activate]
]

{ #category : '*Morphic-Widgets' }
PasteUpMorph >> navigateWindowForward [
	"Change the active window to the next window."

	self nextWindow ifNotNil: [:m |
		self currentWindow ifNotNil: [:w | w sendToBack].
		m isCollapsed ifTrue: [m collapseOrExpand].
		m activate]
]

{ #category : '*Morphic-Widgets' }
PasteUpMorph >> nextVisibleWindow [
	"Answer the next (visible) window to navigate to."

	^self visibleSystemWindows
		ifEmpty: [ nil ]
		ifNotEmpty: [ :windows |
			windows after: self currentWindow ifAbsent: [windows first]
		]
]

{ #category : '*Morphic-Widgets' }
PasteUpMorph >> nextWindow [
	"Answer the next window to navigate to."

	|sys|
	sys := self systemWindows.
	sys ifEmpty: [^nil].
	^sys after: self currentWindow ifAbsent: [sys first]
]

{ #category : '*Morphic-Widgets' }
PasteUpMorph >> previousWindow [
	"Answer the previous window to navigate to."

	|sys|
	sys := self systemWindows.
	sys ifEmpty: [^nil].
	^sys before: self currentWindow ifAbsent: [sys last]
]

{ #category : '*Morphic-Widgets' }
PasteUpMorph >> systemWindows [
	"Answer the system windows in the world."

	^self submorphsSatisfying: [:m | m isSystemWindow]
]

{ #category : '*Morphic-Widgets' }
PasteUpMorph >> visibleSystemWindows [
	"Answer the visible system windows in the world."

	^self submorphsSatisfying: [:m | m isSystemWindow and: [m visible]]
]
