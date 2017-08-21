subclass creation
subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat
	^ self
		subclass: t
		uses: {}
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat