column handling
columnResizersToFront
	"Column resizers should always be at top"
	self columnResizers do: [:cl | cl comeToFront; fillStyle: cl normalFillStyle].
