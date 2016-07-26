copying
copyReplaceFrom: start to: stop with: replacement

	^(self copyFrom: 1 to: start - 1)
		, replacement 
		, (self copyFrom: stop + 1 to: self size)