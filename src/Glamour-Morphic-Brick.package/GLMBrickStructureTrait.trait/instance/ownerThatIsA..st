brick-structure
ownerThatIsA: aClass
	"Return the first enclosing morph that is a kind of aClass, or nil if none"

	^ self firstOwnerSuchThat: [:m | m isKindOf: aClass]