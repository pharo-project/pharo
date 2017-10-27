private
title

	^ self model
		ifNil: [ super title ]
		ifNotNil: [:m | m title ]