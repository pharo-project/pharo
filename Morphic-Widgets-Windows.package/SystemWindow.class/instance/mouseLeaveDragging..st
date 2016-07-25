events
mouseLeaveDragging: evt
	"lock children after drop operations"
	(self isTopWindow not and:[evt hand hasSubmorphs]) ifTrue:[
		self lockInactivePortions.
		evt hand removeMouseListener: self.
	].