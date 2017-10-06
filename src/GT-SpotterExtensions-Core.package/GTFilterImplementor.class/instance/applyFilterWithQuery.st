private
applyFilterWithQuery 
	super applyFilterWithQuery.
	self filteredItems: (self filteredItems reject: [:each | each isFromTrait]).