protocol
removeAll
	"Note: I'm not sure about the semantics here, because Collections normally don't return anything on #removeAll, even though one might expect the original content. Use with care."

	value removeAll.
	self valueRemoved: nil