classes and traits
removeClassNamed: aName
	"Invoked from fileouts:  if there is currently a class in the system named aName, then remove it"

	self at: aName asSymbol ifPresent: [:oldClass | oldClass removeFromSystem]