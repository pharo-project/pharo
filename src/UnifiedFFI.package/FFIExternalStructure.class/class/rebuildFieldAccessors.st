private
rebuildFieldAccessors

	Author useAuthor: 'UFFIGenerator' during: [
		self removeAllOffsetVariables. 
		self 
			compileFields: self fields 
			withAccessors: true]