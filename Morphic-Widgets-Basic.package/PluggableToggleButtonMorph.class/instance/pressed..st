private
pressed: aBoolean
	pressed := aBoolean.
	self update: (getLabelSelector ifNil: [ getIconSelector ])