structure
nearestOwnerThat: conditionBlock
	"Return the first enclosing morph for which aBlock evaluates to true, or nil if none"

	^ self firstOwnerSuchThat: conditionBlock
