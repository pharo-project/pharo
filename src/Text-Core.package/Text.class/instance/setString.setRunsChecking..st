private
setString: aString setRunsChecking: aRunArray
	"Check runs and do the best you can to make them fit..."

	string := aString.
	"check the runs"
	aRunArray ifNil: [^ aString asText].
	(aRunArray isKindOf: RunArray) ifFalse: [^ aString asText].
	aRunArray runs size = aRunArray values size ifFalse: [^ aString asText].
	aRunArray size = aString size ifFalse: [^ aString asText].
	
	runs := aRunArray.