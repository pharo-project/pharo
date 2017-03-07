stepping
wantsSteps
	"Answer whether this presentation wants stepping. Stepping is controlled by the morph
	and sends to the presentation the message #step at intervals controlled by #stepTime."
	^ wantsSteps ifNil: [ false ]