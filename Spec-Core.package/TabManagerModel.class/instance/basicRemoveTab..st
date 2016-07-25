private
basicRemoveTab: aTab
	"BEWARE: this code is an exception. One should never access directly a value holder contents"

	"I do this here for optimisation. No need to trigger the whole collection changed. Only the added element is interesting "

	"This method simply removes the tab from the collection without triggering further updates. It is useful for Adapters if there are different mechanisms of tab removal."

	(tabs value remove: aTab ifAbsent: [ ^ self ]) owner: nil