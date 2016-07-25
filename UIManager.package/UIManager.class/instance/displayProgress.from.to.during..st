display
displayProgress: titleString from: minVal to: maxVal during: workBlock
	"SystemProgressMorph show: titleString from: minVal to:  during: "

	^ workBlock asJob
			title: titleString;
			min: minVal;
			max: maxVal;
			run.