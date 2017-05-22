accessing
packageActionsFor: icePackage
	^ (icePackage packageName beginsWith: self class baselinePrefix) 
		ifTrue: [ { self installDefaultBaselineAction. self installGroupsBaselineAction } ]
		ifFalse: [ #() ]