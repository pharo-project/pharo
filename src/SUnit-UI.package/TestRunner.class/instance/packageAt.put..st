accessing-packages
packageAt: anInteger put: aBoolean
	packagesSelected := packagesSelected
		perform: (aBoolean ifTrue: [ #copyWith: ] ifFalse: [ #copyWithout: ])
		with: (packages at: anInteger ifAbsent: [ ^ self ]).
	self changed: #packageSelected; updateClasses.