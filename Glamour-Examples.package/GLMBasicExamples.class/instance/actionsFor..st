private
actionsFor: aPresentation 
	|actions|
	aPresentation isMultiple 
							ifTrue: [actions := (aPresentation selection) ifNil: [OrderedCollection new]]
							ifFalse: [actions := OrderedCollection with: aPresentation selection ].
	^ actions collect: [:i |
					(GLMGenericAction new) 
						action: [:each | self inform: 'Menu - Item', i printString]; 
						title: 'Menu - Item', i printString; 
						yourself ]