font lookup
fontInfoFor: aLogicalFont familyName: familyName
	| family member |
	
	"use tempFileInfos if not nil, i.e. during an update" 
	"^self fontInfoFor: aLogicalFont in: (tempFileInfos ifNil:[fileInfos]) "
	family := self families at: familyName ifAbsent:[].
	family ifNil:[^nil].
	member := family 
		closestMemberWithStretchValue: aLogicalFont stretchValue 
		weightValue: aLogicalFont weightValue 
		slantValue: aLogicalFont slantValue.
	member ifNil:[^nil].
	^member fileInfo