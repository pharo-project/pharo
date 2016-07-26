instance-creation
newSrcMorph
	"Answer a new src text morph."

	^ self sourceTextModel newScrolledText
		vScrollbarShowNever;
		beForSmalltalkCode;
		beNotWrapped;
		beReadOnly;
		yourself