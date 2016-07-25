stop conditions
endOfRun
	"The end of a run in the display case either means that there is actually 
	a change in the style (run code) to be associated with the string or the 
	end of this line has been reached."
	| runLength |
	lastDisplayableIndex := lastIndex.
	lastIndex = line last ifTrue: [^true].
	runLength := text runLengthFor: (lastIndex := lastIndex + 1).
	runStopIndex := lastIndex + (runLength - 1) min: line last.
	"differ reset of stopConditions and font AFTER the dispaly of last scanned string"
	stopConditionsMustBeReset := true.
	^ false