accessing
presentations
	<debuggingStone: #presentationCreation label: 'Presentation creation'>
	| presentations |
	presentations := GLMCompositePresentation new.
	self presentationsFactory glamourValue: (
			presentations asGlamorousMultiValue, 
			(self transmission origins collect: #value )).
	^ presentations