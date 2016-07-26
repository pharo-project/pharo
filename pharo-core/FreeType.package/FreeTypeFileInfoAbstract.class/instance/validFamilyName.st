accessing
validFamilyName
	"answer the receiver's familyName, or an alternative 
	name to use if the familyName is invalid for some reason"
	
	(familyName copyWithout: $? ) 
		ifEmpty:[
			"workaround problem with FreeType 2.2.1 and MS Gothic, MS Mincho
			where familyName is not read correctly. This may be fixed in later versions
			of FreeType"
			self baseName asUppercase = 'MSGOTHIC'
				ifTrue:[
					index = 0 ifTrue:[^'MS Gothic'].
					index = 1 ifTrue:[^'MS PGothic'].
					index = 2 ifTrue:[^'MS UI Gothic']].
			self baseName asUppercase = 'MSMINCHO'
				ifTrue:[
					index = 0 ifTrue:[^'MS Mincho'].
					index = 1 ifTrue:[^'MS PMincho'].				
			^self baseName asUppercase, ' ', index asString]].
	^familyName