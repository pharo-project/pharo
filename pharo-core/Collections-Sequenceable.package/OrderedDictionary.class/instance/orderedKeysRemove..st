private
orderedKeysRemove: aRemovedKey
	| index |

	index := self orderedKeysIndexOf: aRemovedKey.

	"shift every remaining key after to the left by one"
	orderedKeys
		replaceFrom: index
		to: self size
		with: orderedKeys
		startingAt: index + 1.

	"one key was removed and the rest shifted, so nil what was the last
	key slot before removing and shifting"
	orderedKeys
		at: self size + 1
		put: nil.