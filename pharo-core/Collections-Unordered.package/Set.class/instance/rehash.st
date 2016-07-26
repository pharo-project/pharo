private
rehash
	| newSelf |
	newSelf := self species new: self size.
	self do: [:each | newSelf noCheckAdd: each].
	array := newSelf array