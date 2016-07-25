instance-creation
newDstMorph
	"Answer a new dst text morph."

	^ self destTextModel newScrolledText
		vScrollbarShowNever;
		beForSmalltalkCode;
		beNotWrapped;
		beReadOnly;
		yourself