private
cellOrColumn 
	 
	^cell ifNil: [cell := GLMCustomColumn new browser: self]