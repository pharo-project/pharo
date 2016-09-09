running
checkPackage: aPackage
	renrakuRule class checksPackage ifFalse: [ ^ self ].
	renrakuRule
		check: aPackage
		forCritiquesDo: [ :crit |
			^ result addPackage: aPackage ]