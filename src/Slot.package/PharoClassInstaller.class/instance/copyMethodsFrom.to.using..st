migrating
copyMethodsFrom: oldClass to: newClass using: classModification
	methodUpdateStrategy
		transform: oldClass
		to: newClass
		using: classModification methodModification