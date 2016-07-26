enumerating
withParents
	| paths |
	paths := OrderedCollection new.
	1 to: self size -1 do: [ :index | paths add: ((self class new: index) copyFrom: self) ].
	paths add: self.
	
	^ paths