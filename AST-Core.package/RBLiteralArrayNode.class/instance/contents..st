initialize-release
contents: aRBLiteralNodeCollection 
	contents := aRBLiteralNodeCollection.
	contents do: [:each | each parent: self]