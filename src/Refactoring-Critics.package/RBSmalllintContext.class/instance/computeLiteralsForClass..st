private
computeLiteralsForClass: aClass 
	(selectors addAll: aClass selectors) do: 
			[:sel | 
			self computeLiteralsForSelector: sel in: aClass.
			Processor yield]