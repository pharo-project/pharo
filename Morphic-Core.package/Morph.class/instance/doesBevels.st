accessing
doesBevels
	"To return true means that this object can show bevelled borders, and
	therefore can accept, eg, #raised or #inset as valid borderColors.
	Must be overridden by subclasses that do not support bevelled borders."

	^ false