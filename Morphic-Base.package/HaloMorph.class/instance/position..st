geometry
position: pos
	"Halos display imprefectly if their coordinates are non-integral
		-- especially the direction handles."

	^ super position: pos asIntegerPoint