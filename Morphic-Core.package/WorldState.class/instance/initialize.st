initialization
initialize

	super initialize.
	hands := Array new.
	damageRecorder:= DamageRecorder new.
	stepList := Heap sortBlock: self stepListSortBlock.
	lastStepTime := 0.
	lastAlarmTime := 0.
	self viewBox: Display boundingBox