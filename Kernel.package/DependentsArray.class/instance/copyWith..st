copying
copyWith: newElement 
	"Re-implemented to not copy any niled out dependents."
	| copy i |
	copy := self class new: self size + 1.
	i := 0.
	self do: [:item | copy basicAt: (i:=i+1) put: item].
	copy basicAt: (i:=i+1) put: newElement.
	^copy