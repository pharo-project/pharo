gc
setGCParameters
	"Adjust the VM's default GC parameters to avoid too much tenuring.
	 Maybe this should be left to the VM?"

	| proportion edenSize survivorSize averageObjectSize numObjects |
	proportion := 0.9. "tenure when 90% of pastSpace is full"
	edenSize := self parameterAt: 44.
	survivorSize := edenSize / 5.0. "David's paper uses 140Kb eden + 2 x 28kb survivor spaces; Spur uses the same ratios :-)"
	averageObjectSize := 8 * self wordSize. "a good approximation"
	numObjects := (proportion * survivorSize / averageObjectSize) rounded.
	self tenuringThreshold: numObjects  "tenure when more than this many objects survive the GC"