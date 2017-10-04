accessing
rootsItems
	^ rootItem 
		ifNotNil: #children
		ifNil: [ #() ]