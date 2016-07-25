querying
availableRows

	^expanded ifTrue: [ 
		children
			inject: 1 
			into: [ :sum :each | sum + each availableRows ] 
			
	] ifFalse: [ 1 ]