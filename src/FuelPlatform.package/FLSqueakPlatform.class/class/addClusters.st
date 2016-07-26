private-hacks
addClusters
	Smalltalk
		at: #FLMethodContextCluster
		ifPresent: [ :x | ^ self ].
		
	FLVariableObjectCluster subclass: #FLMethodContextCluster
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self hacksCategoryName