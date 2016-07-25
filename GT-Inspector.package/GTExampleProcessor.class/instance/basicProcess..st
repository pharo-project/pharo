private
basicProcess: anExample 
	^ self 
		process: anExample 
		withArguments: [ anExample arguments collect: [ :each | self process: each example ] ] 