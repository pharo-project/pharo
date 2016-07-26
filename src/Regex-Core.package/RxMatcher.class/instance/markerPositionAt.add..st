privileged
markerPositionAt: anIndex add: position
	"Remember position of another instance of the given marker."

	(markerPositions at: anIndex) addFirst: position