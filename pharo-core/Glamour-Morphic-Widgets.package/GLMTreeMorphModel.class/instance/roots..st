accessing
roots: anObjectOrCollection
	roots := anObjectOrCollection asOrderedCollection 
				collect: [:each | self nodeModelFor: each ]