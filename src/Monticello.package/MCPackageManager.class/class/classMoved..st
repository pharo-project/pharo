system changes
classMoved: anEvent
	self managersForPackage: anEvent oldPackage do: [ :mgr | mgr modified: true ].
	self managersForPackage: anEvent newPackage do: [ :mgr | mgr modified: true ].