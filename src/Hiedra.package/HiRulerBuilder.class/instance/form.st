building
form
	| surface form |
	surface := self athensSurface.

	form := Form 
		extent: (surface width @ surface height) 
		depth: 32.

	form getCanvas 
		drawImage: surface asForm 
		at: 0@0.

	^ form