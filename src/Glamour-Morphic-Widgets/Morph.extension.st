Extension { #name : #Morph }

{ #category : #'*glamour-morphic-widgets' }
Morph >> glamourMinExtent [

	^ self minExtent
]

{ #category : #'*glamour-morphic-widgets' }
Morph >> glamourOptimalExtent [

	|tmpOldResizing tmpResult|
	tmpOldResizing := self setOptimalResizingStrategyAndReturnOldOne. 
	tmpResult := self minExtent.
	self returnToOldResizingStrategy:  tmpOldResizing.
	^ tmpResult 
]

{ #category : #'*glamour-morphic-widgets' }
Morph >> returnToOldResizingStrategy: aCollection [

	self submorphs with: aCollection third do: [:aMorph :resizingParameter |
		aMorph returnToOldResizingStrategy: resizingParameter.
		].
	self hResizing: aCollection first.
	self vResizing: aCollection second.
]

{ #category : #'*glamour-morphic-widgets' }
Morph >> setOptimalResizing [
	self hResizing: #shrinkWrap.
	self vResizing: #shrinkWrap
]

{ #category : #'*glamour-morphic-widgets' }
Morph >> setOptimalResizingStrategyAndReturnOldOne [
	| tmpHResizing tmpVResizing tmpSubmorphResizing |
	tmpHResizing := self hResizing.
	tmpVResizing := self vResizing.
	self setOptimalResizing.
	tmpSubmorphResizing := self submorphs collect: [ :aMorph | aMorph setOptimalResizingStrategyAndReturnOldOne ].
	^ {tmpHResizing.
	tmpVResizing.
	tmpSubmorphResizing}
]
