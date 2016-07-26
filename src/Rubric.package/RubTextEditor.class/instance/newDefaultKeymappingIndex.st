keymapping
newDefaultKeymappingIndex
	defaultKeymappingIndex := IdentityDictionary new.
	defaultKeymappingIndex at: #command put: self defaultCommandKeymapping.
	defaultKeymappingIndex at: #shiftCommand put: self defaultShiftCommandKeymapping.
	^ defaultKeymappingIndex 