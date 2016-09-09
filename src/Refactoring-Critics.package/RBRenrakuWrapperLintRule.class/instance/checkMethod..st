running
checkMethod: aMethod

	renrakuRule class checksNode ifTrue: [ 
		^ self byNodeCheckMethod: aMethod ].
	renrakuRule class checksMethod ifFalse: [ ^ self ].
	renrakuRule
		check: aMethod
		forCritiquesDo: [ :crit |
			result addMethod: aMethod.
			self
				addSearchStringFrom: crit
				in: aMethod definition.
			^ self ]
	