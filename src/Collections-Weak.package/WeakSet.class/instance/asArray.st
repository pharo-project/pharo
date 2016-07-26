converting
asArray

	| newArray |
	
	newArray := OrderedCollection new.
	
	self do: [:each | newArray add: each].
	
	^ newArray asArray