private
selectedGroup
	
	^ self selectedNode 
		ifNil: [ nil ]
		ifNotNil: [:node |
			node isGroup
				ifTrue: [ node item ]
				ifFalse: [ node parentNode item ]]