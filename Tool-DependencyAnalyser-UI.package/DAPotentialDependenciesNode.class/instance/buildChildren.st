api
buildChildren
	^ (self messageSendAnalysis differenceWithManuallyResolvedDependencies
		collect: [ :selector | 
						(DAMissingMethodNode new
								content: selector;
								parentNode: self;
								yourself) ]
		as: Array) 
			sort: [ :a :b | a content < b content ]