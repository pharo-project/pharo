accessing
createPath: aBlock
	
	self newPath.
	"set default to relative"
	absolute := false.
	endPoint := ZeroPoint.
	
	"set the implicit path origin"
	self moveToX: 0 Y: 0.
		
	aBlock value: self.
	
	^ self copyPath initialize 