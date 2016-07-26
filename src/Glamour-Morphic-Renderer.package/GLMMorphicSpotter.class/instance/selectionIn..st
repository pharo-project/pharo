callbacks
selectionIn: singleSpotlight

	(self childrenMorphs reject: [:e| e = singleSpotlight] ) do: [:e| 
			e deselect.
		]