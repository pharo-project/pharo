private
evaluate: aBlock onBehalfOf: aProcess
	"Evaluate aBlock setting effectiveProcess to aProcess.  Used
	 in the execution simulation machinery to ensure that
	 Processor activeProcess evaluates correctly when debugging."
	| oldEffectiveProcess |
	oldEffectiveProcess := effectiveProcess.
	effectiveProcess := aProcess.
	^aBlock ensure: [effectiveProcess := oldEffectiveProcess]