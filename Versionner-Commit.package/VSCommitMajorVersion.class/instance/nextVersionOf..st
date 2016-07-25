accessing
nextVersionOf: aProject
	| version |
	version := self semanticVersionFor: aProject.
	^ '{1}.0' format: { 
		(version at: 1) + 1 }