private
rawResultsDo: aBlockClosure 
	self revwalk 
		select: [ :result | self shouldInclude: result ]
		thenDo: aBlockClosure