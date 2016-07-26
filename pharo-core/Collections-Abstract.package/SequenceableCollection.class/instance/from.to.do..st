enumerating
from: start to: stop do: aBlock
	"Evaluate aBlock for all elements between start and stop (inclusive)."

	start to: stop do: [:index | aBlock value: (self at: index)]