accessing
headReference
	"Answer a reference to the last entry added to this store."

	^ headReference ifNil: [
		headReference := self store headReference ]