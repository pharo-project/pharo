running
check: aClass forCritiquesDo: aCriticBlock

	aClass slots do: [ :slot | 
		self
			check: slot
			of: aClass
			forCritiquesDo: aCriticBlock ]