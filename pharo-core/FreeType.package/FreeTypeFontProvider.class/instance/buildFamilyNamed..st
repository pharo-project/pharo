font families
buildFamilyNamed: aFamilyGroupName
	| infos family|
	family := FreeTypeFontFamily new
		familyName: aFamilyGroupName;
		yourself. 
	infos := fileInfos select:[:each | each familyGroupName = aFamilyGroupName].
	family addMembersFromFileInfos: infos.
	family addSimulatedMembers.
	^family
		
			
			
		