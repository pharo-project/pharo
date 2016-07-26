accessing
selectedItems
	^ self selectedNodes collect: [:n | n item]
