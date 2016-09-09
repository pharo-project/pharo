running
checkClass: aClass
	renrakuRule class checksClass ifFalse: [ ^ self ].
	renrakuRule
		check: aClass
		forCritiquesDo: [ :crit |
			result addClass: aClass.
			self
				addSearchStringFrom: crit
				in: aClass definition.
			^ self ]