protected
setDefaultAnalysis

	analysisFactory := [:anObject | 
		(FLAnalysis 
			newWith: self firstInMapperChain 
			private: self privateFirstInMapperChain
			root: anObject)
			run;
			yourself ]