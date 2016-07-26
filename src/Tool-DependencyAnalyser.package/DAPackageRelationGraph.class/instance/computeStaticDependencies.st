adding
computeStaticDependencies
	self packages
		do: [ :each | 
			each isSeen
				ifFalse: [ 
					self computeStaticDependencies: each.
					each beSeen ] ] 