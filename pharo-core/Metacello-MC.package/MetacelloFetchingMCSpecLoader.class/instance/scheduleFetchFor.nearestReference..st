private
scheduleFetchFor: packageSpec nearestReference: reference
	"latest version in repository already matches the cached reference...no need to schedule fetch"

	^ self