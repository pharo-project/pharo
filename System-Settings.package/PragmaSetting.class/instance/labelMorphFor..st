user interface
labelMorphFor: aContainer
	^ StringMorph contents: (aContainer model viewedLabelOfSetting: self)