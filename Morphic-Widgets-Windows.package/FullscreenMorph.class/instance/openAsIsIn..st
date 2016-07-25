open/close
openAsIsIn: aWorld
	"Start stepping."
	
	aWorld addMorph: self.
	(self submorphs notEmpty and: [self submorphs first isSystemWindow])
		ifTrue: [self submorphs first openedFullscreen].
	aWorld startSteppingSubmorphsOf: self.
	
	self announceOpened.