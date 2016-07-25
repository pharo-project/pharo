operations
activate
	"Update the non visible tabs too."

	super activate.
	self nonVisibleTabs do: [:t | t activate]