scope
popScope
	"Propogate free var usages to their outer vars, then return outer scope"

	^ self outerScope