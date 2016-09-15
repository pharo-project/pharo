accessing
destinationsBlock: aBlockClosure 
	self 
		deprecated: 'Use targetsBlock: instead'
		on: '30 March 2016' 
		in: 'Pharo-5.0-50571-'.
	^ self targetsBlock: aBlockClosure