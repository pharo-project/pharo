selected
setPointSizeListFrom: aFontFamilyMember
	| style old new |
	old := pointSizeList.
	(aFontFamilyMember isKindOf: FontFamilyMemberAbstract)
		ifTrue:[
			style := TextStyle named: aFontFamilyMember family familyName.
			style ifNotNil:[
				new := style pointSizes collect: [:each | 
					each reduce asString padLeftTo: 3]]].
	new ifNil: [ new := (1 to: 256) collect: [:each | 
				each asString padLeftTo: 3 ]].
	
	pointSizeList := new.
	old ~= new ifTrue: [ self announcer announce: #pointSizeListChanged]