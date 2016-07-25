private
setConditionArray: aSymbol

	aSymbol == #paddedSpace ifTrue: [^stopConditions := PaddedSpaceCondition "copy"].
	"aSymbol == #space ifTrue: [^stopConditions := SpaceCondition copy]."
	aSymbol == nil ifTrue: [^stopConditions := NilCondition "copy"].
	self error: 'undefined stopcondition for space character'.
