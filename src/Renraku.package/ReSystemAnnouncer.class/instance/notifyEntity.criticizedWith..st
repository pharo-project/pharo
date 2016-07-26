announce
notifyEntity: anEntity criticizedWith: critiques

	self announce: (
		ReEntityCriticized new 
			critiques: critiques;
			entity: anEntity;
			yourself)
