tests
testCapacity
	"The current Dictionary implementation allocates more than specified.
	The amount allocated may change in the future but it likely won't ever
	be less than specified, so a >= test is used throughout."

	| defaultCapacity dictionary |

	defaultCapacity := self defaultCapacity.
	dictionary := self dictionaryClass new.
	self assert: dictionary capacity >= defaultCapacity.

	dictionary := self dictionaryClass new: (defaultCapacity / 2) asInteger.
	self assert: dictionary capacity >= (defaultCapacity / 2) asInteger.

	dictionary := self dictionaryClass newFrom: self orderedAssociations.
	self assert: dictionary capacity >= self orderedAssociations size.

	self orderedAssociations size to: 0 by: -1 do: [:i |
		self
			shouldnt: [
				dictionary := self dictionaryClass new: i.
				dictionary addAll: self orderedAssociations]
			raise: Error].