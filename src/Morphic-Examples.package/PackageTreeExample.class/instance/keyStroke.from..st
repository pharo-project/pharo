as yet unclassified
keyStroke: anEvent from: aTreeMorph 
	self selectedNode
		ifNotNil: [:current | current keyStroke: anEvent from: aTreeMorph]