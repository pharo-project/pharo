rendering
cachedPresentation
	"currentPresentation applies the transformation dynamically.
	This means that calling it twice, will retrieve two different objects.
	To solve the problem, cachedPresentation stores the last currentPresentation, and 
	it can be used for further inspection"
	^ cachedPresentation