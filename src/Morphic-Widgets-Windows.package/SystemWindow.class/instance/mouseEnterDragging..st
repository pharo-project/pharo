events
mouseEnterDragging: evt
	"unlock children for drop operations"
	(self isTopWindow not and:[evt hand hasSubmorphs]) ifTrue:[
		self submorphsDo:[:m| m unlock].
		evt hand addMouseListener: self. "for drop completion on submorph"
	].