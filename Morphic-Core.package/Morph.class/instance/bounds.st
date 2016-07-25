geometry
bounds
	"Return the bounds of this morph."
	"Note: It is best not to override this method because many methods in Morph and its subclasses use the instance variable directly rather than 'self bounds'. Instead, subclasses should be sure that the bounds instance variable is correct."

	^ bounds
