testing-basic
testInvalidComposition
	self t1 @ {(#a -> #b)} @ {(#x -> #y)}.
	(self t1 + self t2) @ {(#a -> #b)} @ {(#x -> #y)}.
	self t1 - {#a} - {#b}.
	self t1 + self t2 - {#a} - {#b}.
	self should: [ (self t1 - {#x}) @ {(#a -> #b)} ] raise: TraitCompositionException.
	self should: [ (self t1 + self t2 - {#x}) @ {(#a -> #b)} ] raise: TraitCompositionException.
	self should: [ self t1 + self t1 ] raise: TraitCompositionException.
	self should: [ (self t1 + self t2) @ {(#a -> #b)} + self t1 ] raise: TraitCompositionException.
	self
		should: [ 
			self t1
				@
					{(#a -> #m11).
					(#a -> #m12)} ]
		raise: TraitCompositionException.
	self
		should: [ 
			self t1
				@
					{(#a -> #m11).
					(#b -> #a)} ]
		raise: TraitCompositionException