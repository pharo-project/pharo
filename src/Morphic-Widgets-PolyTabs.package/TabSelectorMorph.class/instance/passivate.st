operations
passivate
	"Update the non visible tabs too."

	super passivate.
	self nonVisibleTabs do: [:t | t passivate]