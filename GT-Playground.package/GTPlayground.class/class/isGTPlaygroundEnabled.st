settings
isGTPlaygroundEnabled

	^ GTPlaygroundEnabledStatus ifNil: [ GTPlaygroundEnabledStatus := Smalltalk tools workspace = self ]