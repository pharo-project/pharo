accessing
handOutItems
	"we don't want to hold a reference to the items after they are hand out to the registration"

	| itemsCopy |
	itemsCopy := items copy.
	items removeAll.
	^ itemsCopy