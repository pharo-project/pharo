enumerating
paddedWith: otherCollection do: twoArgBlock 
	"Evaluate twoArgBlock with corresponding elements from this collection and otherCollection.
	Missing elements from either will be passed as nil."
	1 to: (self size max: otherCollection size) do:
		[:index | twoArgBlock value: (self at: index ifAbsent: [])
				value: (otherCollection at: index ifAbsent: [])]