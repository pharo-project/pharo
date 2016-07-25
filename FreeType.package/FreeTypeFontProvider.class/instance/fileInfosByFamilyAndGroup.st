accessing
fileInfosByFamilyAndGroup
	"Answer a Dictionary of Dictionaries of Sets.
	familyName->familyGroupName->Set(FreeTypeFileInfo)
	
	self current fileInfosByFamilyAndGroup
	"
	| answer |
	answer := Dictionary new.
	"file could be in fileInfos twice?
	need to only process once, need directory precedence?"
	fileInfos do:[:info | | group family |
		family := answer at: info familyName ifAbsentPut:[Dictionary new].
		group := family at: info familyGroupName ifAbsentPut: [OrderedCollection new].
		group 
			detect:[:each| 
				each bold = info bold
				and:[ each italic = info italic
				and:[ each fixedWidth = info fixedWidth
				and:[ each postscriptName = info postscriptName
				and:[ each styleName = info styleName ]]]]]
			ifNone:[ group add: info]].
	^ answer	