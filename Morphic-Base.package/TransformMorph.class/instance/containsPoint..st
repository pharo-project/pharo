geometry testing
containsPoint: aPoint
	(bounds containsPoint: aPoint) ifFalse: [^ false].
	self hasSubmorphs
		ifTrue: [ | localPoint |  localPoint := (transform globalPointToLocal: aPoint) .
				self submorphsDo: 
					[:m | (m containsPoint: localPoint)
							ifTrue: [^ true]].
				^ false]
		ifFalse: [^ true]