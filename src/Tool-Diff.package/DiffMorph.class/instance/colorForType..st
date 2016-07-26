accessing colors
colorForType: type
	"Anwser the color to use for the given change type."

	^ {self matchColor. self additionColor. self removalColor. self modificationColor}
		at: (#(match addition removal modification) indexOf: type)