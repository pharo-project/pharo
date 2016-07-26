private
rehash
 	| newSelf |
 	newSelf := self species new: self size.
 	newSelf keyBlock: keyBlock.
 	self do: [:each | newSelf noCheckAdd: each].
 	array := newSelf array