handling
resumeUnchecked: resumptionValue
	"Return resumptionValue as the value of #signal, unless this was called after an #outer message, then return resumptionValue as the value of #outer."

	| ctxt |
	outerContext 
		ifNil: [
			signalContext return: resumptionValue ] 
		ifNotNil: [
			ctxt := outerContext.
			outerContext := ctxt tempAt: 1. "prevOuterContext in #outer"
			ctxt return: resumptionValue ]
