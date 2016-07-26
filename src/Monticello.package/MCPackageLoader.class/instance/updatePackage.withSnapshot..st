public
updatePackage: aPackage withSnapshot: aSnapshot
	|  patch packageSnap |
	packageSnap := aPackage snapshot.
	patch := aSnapshot patchRelativeToBase: packageSnap.
	patch applyTo: self.
	packageSnap definitions do: [:ea | self provisions addAll: ea provisions]
