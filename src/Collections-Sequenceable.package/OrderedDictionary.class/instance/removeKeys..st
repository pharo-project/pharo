removing
removeKeys: aKeyCollection
	"Fast removal of multiple keys; returns self to avoid
	having to create a removed value collection and does not
	raise errors."

	aKeyCollection	size > 1
		ifTrue: [| oldSize newOrderedKeys newOrderedKeysIndex |
			oldSize := self size.
			aKeyCollection do: [:each |
				dictionary
					removeKey: each
					ifAbsent: [nil]].

			newOrderedKeys := Array new: oldSize.
			newOrderedKeysIndex := 0.
			1 to: oldSize do: [:i | | key |
				(dictionary includesKey: (key := orderedKeys at: i))
					ifTrue: [
						newOrderedKeys
							at: (newOrderedKeysIndex := newOrderedKeysIndex + 1)
							put: key]].

			orderedKeys := newOrderedKeys]
		ifFalse: [
			aKeyCollection size = 1
				ifTrue: [
					"use #anyOne, because it can be a Set"
					self
						removeKey: aKeyCollection anyOne
						ifAbsent: [nil]]]