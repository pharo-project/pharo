initialize
initialize
	"self initialize"
	
	EncodedCharSets := Array new: 256.
	self allSubclassesDo: [:each | each initialize].
	LanguageEnvironment allSubclassesDo:  [:each | each initialize].


	"EncodedCharSets at: 0+1 put: Unicode.
	EncodedCharSets at: 1+1 put: JISX0208.
	EncodedCharSets at: 2+1 put: GB2312.
	EncodedCharSets at: 3+1 put: KSX1001.
	EncodedCharSets at: 4+1 put: JISX0208.
	EncodedCharSets at: 5+1 put: JapaneseEnvironment.
	EncodedCharSets at: 6+1 put: SimplifiedChineseEnvironment.
	EncodedCharSets at: 7+1 put: KoreanEnvironment.
	EncodedCharSets at: 8+1 put: GB2312.
	EncodedCharSets at: 12+1 put: KSX1001.
	EncodedCharSets at: 13+1 put: GreekEnvironment.
	EncodedCharSets at: 14+1 put: Latin2Environment.
	EncodedCharSets at: 15+1 put: RussianEnvironment.
	EncodedCharSets at: 17+1 put: Latin9Environment.
	EncodedCharSets at: 256 put: Unicode."
