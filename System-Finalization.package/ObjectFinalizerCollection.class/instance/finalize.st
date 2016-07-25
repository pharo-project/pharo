finalization
finalize
	"Finalize all elements in this collection. The elements are expected to be ObjectFinalizers, but can be any object which understands #finalize."

	self do: #finalize