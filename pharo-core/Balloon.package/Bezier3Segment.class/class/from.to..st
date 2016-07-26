instance creation
from: p1 to: p2
	^ self new from: p1 via: (p1 interpolateTo: p2 at: 0.3333) and: (p1
interpolateTo: p2 at: 0.66667) to: p2