tests - window creation and deletion
testResizingClosing
	| coll |

	window := SystemWindow labelled: 'foo'.
	coll := OrderedCollection new.
	window openInWorld.
	window announcer when: WindowAnnouncement do: [:ann | coll add: ann].
	self assert: (coll isEmpty).
	window minimizeOrRestore.
	
	"Resizing, moving, deActivation, collapsing "
	self assert: (coll size = 4).
	self assert: (coll first isResized).
	self assert: (coll second isMoved).
	self assert: (coll third isDeActivated).
	self assert: (coll fourth isCollapsed).

	window delete.

	self assert: (coll size = 5).
	self assert: (coll fifth isClosed).
	window := nil