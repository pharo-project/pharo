testing
isBinary
	"Answers whether the contents of a resource described by me 
	should be considered as binary instead of textual."

	self main = 'text' ifTrue: [ ^ false ].
	^ SubTypeBinaryCache
		at: self sub
		ifAbsentPut: [ 
			| subTypeFragments |
			subTypeFragments := self sub substrings: '+-'.
			subTypeFragments noneSatisfy: [ :each | 
				self class textSubTypeFragments includes: each ] ]