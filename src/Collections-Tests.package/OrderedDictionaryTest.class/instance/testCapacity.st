tests
testCapacity
	"The current Dictionary implementation allocations more than specified; the amount allocated may
	change in the future, but it likely won't ever be less than specified, so a >= test is used instead."
	| defaultCapacity |

	defaultCapacity := self defaultCapacity.
	self
		assert: (self dictionaryClass new) capacity >= defaultCapacity;
		assert:
			(self dictionaryClass new: (defaultCapacity / 2) asInteger) capacity >=
				(defaultCapacity / 2) asInteger;
		assert:
			(self dictionaryClass newFrom: self dictionaryAssociations)
				capacity >= self dictionaryAssociations size.

	0 to: self dictionaryAssociations size - 1 do: [:i |
		self
			shouldnt: [(self dictionaryClass new: i) addAll: self dictionaryAssociations]
			raise: Error].