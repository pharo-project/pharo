accessing
embeddedWindowOrNil

	"answer nil for common morphs, yourself from system windows and first submorph for transformation morphs"
	
	| s | 
	
	(self submorphs size = 1) ifTrue: [ 
		s := self firstSubmorph.
		s isSystemWindow ifTrue: [ ^ s ] ].
	^ nil.