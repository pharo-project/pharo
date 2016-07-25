initialization
initialize

	sessionHolder := nil asValueHolder.
	actionsCache := nil.
	super initialize.
	
	"self setDefaultFilters."