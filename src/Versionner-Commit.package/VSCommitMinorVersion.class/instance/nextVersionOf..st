accessing
nextVersionOf: aProject
	| version |
	version := self semanticVersionFor: aProject.
	^ '{1}.{2}' format: {
		(version at: 1). 
		(version at: 2) + 1 }