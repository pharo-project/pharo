private - ISO 3166
buildIso3166CodesTables
	"ISOLanguageDefinition buildIso3166CodesTables"
	| rawdata stream country isoa2 isoa3 unNumeric macName macCode windowsName windowsCode empty table |
	rawdata := self iso3166Codes.
	table := OrderedCollection new: 200. 
	stream := rawdata readStream.
	empty := 160 asCharacter asString.
	[stream atEnd] whileFalse: 
		[country := stream nextLine.
		isoa2 := stream nextLine.
		isoa3 := stream nextLine.
		unNumeric := stream nextLine.
		windowsName := stream nextLine.
		windowsName = empty ifTrue: [windowsName := nil].
		windowsCode := stream nextLine. 
		windowsCode = empty ifTrue: [windowsCode := nil].
		macName := stream nextLine.
		macName = empty ifTrue: [macName := nil].
		macCode := stream nextLine.
		macCode = empty ifTrue: [macCode := nil].
		table add: { country.  isoa2. isoa3.  unNumeric. windowsName.  windowsCode.  macName. macCode. }].
	^table