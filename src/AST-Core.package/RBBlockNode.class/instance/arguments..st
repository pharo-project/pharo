accessing
arguments: argCollection 
	arguments := argCollection.
	arguments do: [:each | each parent: self]