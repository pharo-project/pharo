private
targetSnapshotOf: aReference
	^ (aReference resolveWith: self gofer) version snapshot