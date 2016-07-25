private
outOfWorld: aWorld

	super outOfWorld: aWorld.
	
	aWorld removeAlarm: #hideShow for: self.