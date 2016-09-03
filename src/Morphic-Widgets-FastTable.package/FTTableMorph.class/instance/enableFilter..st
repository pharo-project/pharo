accessing
enableFilter: aFTFilterClass
	"Enables filtering. Not compatible with the search."

	self enableFilter.
	function filterClass: aFTFilterClass