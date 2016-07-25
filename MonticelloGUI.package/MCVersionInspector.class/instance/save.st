actions
save
	
	self pickRepository 
		ifNotNil: [ :ea | 
			self version dependencies do: [ :each | ea storeVersion: each resolve ]. 
			ea storeVersion: self version ]