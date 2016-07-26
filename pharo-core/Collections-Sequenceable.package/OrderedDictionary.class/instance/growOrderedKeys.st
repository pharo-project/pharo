private
growOrderedKeys
	orderedKeys :=
		(Array new: ((orderedKeys size * 1.5) asInteger max: 10))
			replaceFrom: 1
			to: orderedKeys size
			with: orderedKeys
			startingAt: 1.