font families
allFamilies
	"answer an Array containing all the font families from the receiver's fontProviders,
	together with any TextStyle font families, sorted by family name"

	| answer textStyleFamilies |
	answer := Set new.
	fontProviders do: [ :each | answer addAll: each families ].
	textStyleFamilies := TextStyle knownTextStylesWithoutDefault
		collect: [ :textStyleName | 
			TextStyleAsFontFamily new
				textStyle: (TextStyle named: textStyleName);
				familyName: textStyleName;
				yourself ].	"reject any textStyles whose defaultFont also appears as a fontFamily"
	textStyleFamilies := textStyleFamilies
		reject: [ :textStyleFamily | 
			| textStyleFamilyName |
			textStyleFamilyName := textStyleFamily textStyle defaultFont familyName.
			answer anySatisfy: [ :fontFamily | fontFamily familyName = textStyleFamilyName ] ].
	answer addAll: textStyleFamilies.
	^ (answer asSortedCollection: [ :a :b | a familyName <= b familyName ]) asArray