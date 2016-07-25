computing types
implementorsOf: aSelector 
	| classes |
	classes := OrderedCollection new.
	self rootClasses do: 
			[:each | 
			self 
				implementorsOf: aSelector
				in: each
				storeIn: classes].
	^classes