copying
processStopReason: reason edge: edge fill: fill
	"The engine has stopped because of some reason.
	Try to figure out how to respond and do the necessary actions."
	"Note: The order of operations below can affect the speed"

	"Process unknown fills first"
	reason = GErrorFillEntry ifTrue:[
		fill source: (externals at: fill index).
		"Compute the new fill"
		fill computeFill.
		"And mix it in the out buffer"
		^self primMergeFill: fill destForm bits from: fill].

	"Process unknown steppings in the AET second"
	reason = GErrorAETEntry ifTrue:[
		edge source: (externals at: edge index).
		edge stepToNextScanLine.
		^self primChangeActiveEdgeTableEntryFrom: edge].

	"Process unknown entries in the GET third"
	reason = GErrorGETEntry ifTrue:[
		edge source: (externals at: edge index).
		edge stepToFirstScanLine.
		^self primAddActiveEdgeTableEntryFrom: edge].

	"Process generic problems last"
	(self canProceedAfter: reason) ifTrue:[^self]. "Okay."

	^self error:'Unkown stop reason in graphics engine'
