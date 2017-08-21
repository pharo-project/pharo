printing
classVariablesString
	"Answer a string of my class variable names separated by spaces."

	^String streamContents: [ :stream | 
		self classVarNames 
			do: [ :each | stream nextPutAll: each ]
			separatedBy: [ stream space ] ]