accessing
arguments: aLiteralCollection 
	arguments := aLiteralCollection.
	arguments do: [ :each | each parent: self ]