accessing
relationGraph: anObject
	| warnings alertMessage |
	warnings := Set new.
	relationGraph := anObject.

	[ self relationGraph build ]
		on: PotentialOutDatedDependencyWarning 
		do: [ :ex | warnings add: ex. ex resume ].
	self buildRoots.
	alertMessage := 'Some dependencies declared in ' ,
		self relationGraph packagesName first,
		' Manifest as manuallyResolvedDependencies not detected as a dependency! Please check :' , String cr.
	warnings 	ifNotEmpty: [ 
		UIManager default 
			alert: alertMessage , (String cr join: (warnings collect: #dependency ))
			title: 'Potential outdated dependencies' ]