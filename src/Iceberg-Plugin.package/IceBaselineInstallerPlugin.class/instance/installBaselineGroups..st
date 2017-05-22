actions
installBaselineGroups: icePackage
	| groupsString groups |
	
	groupsString := self pickGroupsOnCancel: [ ^ self ].
	groups := groupsString 
		ifNotNil: [ (groupsString substrings: ',') collect: #trimBoth as: Array ]
		ifNil: [ #() ].
	self installBaseline: icePackage groups: groups