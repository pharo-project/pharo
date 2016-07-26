accessing
doItContext: aOneArgBlock
	"The block will be passed the current entity and is expected to return a context. Any code evaluated from the presentation will be executed as part of the retuned context."
	doItContext := aOneArgBlock