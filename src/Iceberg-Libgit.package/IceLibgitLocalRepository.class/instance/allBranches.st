querying
allBranches
	self withRepoDo: [ :repo | 	
		^ OrderedCollection new: 10 streamContents: [ :stream |
			repo allBranchesDo: [ :br :isLocal | 
				stream nextPut: (
					IceBranch 
						named: br name 
						local: isLocal
						inRepository: self frontend) ]]]
	
