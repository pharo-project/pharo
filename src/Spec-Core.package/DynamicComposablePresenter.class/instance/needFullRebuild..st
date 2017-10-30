api
needFullRebuild: aBoolean

	self needRebuild: aBoolean.
	self widgetsDo: [:e | e needRebuild: aBoolean ]