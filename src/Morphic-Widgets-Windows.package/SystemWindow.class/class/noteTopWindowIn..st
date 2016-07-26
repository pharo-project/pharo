top window
noteTopWindowIn: aWorld
	"Clear the current top window and activate the topmost visible window"
	
	TopWindow := nil.
	
	aWorld ifNil: [^ self].
	aWorld submorphsDo: [:m |
		(m isSystemWindow and: [m isCollapsed not]) ifTrue: [
			^m activate]]