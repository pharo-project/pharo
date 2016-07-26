private
compactWithoutBecome
	"Return a copy of self which has the highest possible load factor (between 37.5% and 75%)."
	
	| newInstance |
	newInstance := self species new: self size.
	1 to: self basicSize do: [ :index | 
		(self basicAt: index) ifNotNil: [ :key |
			newInstance at: key put: (array at: index) ] ].
	^newInstance