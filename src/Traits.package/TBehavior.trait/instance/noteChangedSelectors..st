traits
noteChangedSelectors: aCollection
	"Start update of my methodDict (after changes to traits in traitComposition
	or after a local method was removed from my methodDict). The argument 
	is a collection of method selectors that may have been changed. Most of the time
	aCollection only holds one selector. But when there are aliases involved 
	there may be several method changes that have to be propagated to users."

	| affectedSelectors |
	affectedSelectors := IdentitySet new.
	aCollection do: [:selector |
		affectedSelectors addAll: (self updateMethodDictionarySelector: selector)].
	self notifyUsersOfChangedSelectors: affectedSelectors.
	^ affectedSelectors