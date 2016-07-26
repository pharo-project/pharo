accessing
path
	"Path part ... in the following  http:// server / path /updatesXX.list
	 where path is usually pathX.X including the Pharo version."
	
	^ path ifNil: [ path := self defaultPath ].