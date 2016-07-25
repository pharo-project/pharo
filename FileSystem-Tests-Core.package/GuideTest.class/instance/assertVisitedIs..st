asserting
assertVisitedIs: anArray
	visited with: anArray do:
		[:entry :basename | 
		self assert: entry reference basename = basename]