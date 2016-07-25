initialization
initializeValueHolders
	selectionHolder := SelectionValueHolder new.
	listHolder := Array new asValueHolder.
	sortingBlockHolder := self defaultSortingBlock asValueHolder.
	filteringBlockHolder := self defaultFilteringBlock asValueHolder.
	wrapBlockHolder := [ :object | object asStringOrText ] asValueHolder.
	menuHolder := [ :menu :shifted | nil ] asValueHolder.
	multiSelectionHolder := IdentityDictionary new asValueHolder.
	multiSelection := false asValueHolder.
	allowToSelect := true asValueHolder.
	backgroundColorBlock := [ :item :index | Smalltalk ui theme backgroundColor ] asValueHolder.
	autoDeselect := true asValueHolder.
	listAnnouncer := Announcer new.
