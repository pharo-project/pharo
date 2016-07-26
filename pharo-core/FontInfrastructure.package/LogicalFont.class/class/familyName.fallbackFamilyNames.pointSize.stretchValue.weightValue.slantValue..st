instance creation
familyName: familyName fallbackFamilyNames: fallbackFamilyNames pointSize: pointSize stretchValue: stretch weightValue: weight slantValue: slant

	"^self all asArray"
	"^(self all collect:[:each | each]) asArray"
	^self all
		detect:[:each |
			each familyName = familyName and:[
			each fallbackFamilyNames = fallbackFamilyNames and:[
			each pointSize = pointSize and:[
			each weightValue = weight and:[
			each stretchValue = stretch and:[
			each slantValue = slant]]]]]]
		ifNone:[
			self  new
				familyName: familyName; 
				fallbackFamilyNames: fallbackFamilyNames;
				pointSize: pointSize; 
				weightValue:weight;
				stretchValue: stretch;
				slantValue: slant;
				yourself]