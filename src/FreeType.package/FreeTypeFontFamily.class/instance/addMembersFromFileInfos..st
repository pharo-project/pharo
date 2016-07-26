accessing
addMembersFromFileInfos: aCollectionOfFreeTypeFileInfo
	
	
	aCollectionOfFreeTypeFileInfo do:[:aFileInfo | | member |
		member := FreeTypeFontFamilyMember fromFileInfo: aFileInfo.
		(self memberWithStyleName: member styleName)
			ifNil:[self addMember: member]].
	