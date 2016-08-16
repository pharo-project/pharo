api
buildChildren
	^ (self messageSendAnalysis unimplementedCallsInAllDependencies
		collect: [ :selector | 
						(DAMissingMethodNode new
								content: selector;
								parentNode: self;
								yourself) ]
		as: Array) 
			sort: [ :a :b | a content < b content ]