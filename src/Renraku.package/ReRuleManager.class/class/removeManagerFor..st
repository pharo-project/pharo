event subscriptions
removeManagerFor: anRPackage

	self managers
		removeKey: anRPackage 
		ifAbsent: [  ]
	