initialization
getListElementSelector: aSymbol
	"specify a selector that can be used to obtain a single element in the underlying list"
	self basicGetListElementSelector: aSymbol.
	list := nil.  "this cache will not be updated if getListElementSelector has been specified, so go ahead and remove it"
	self updateList.