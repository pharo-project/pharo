accessing
log

	^ log ifNil: [ 
		log := EpLog newWithStore: self ombuStore ]