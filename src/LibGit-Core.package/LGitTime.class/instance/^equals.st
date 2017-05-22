comparing
= anObject
	^ self == anObject
		or: [ self class == anObject class
			and: [ self time = anObject time
				and: [  self offset = anObject offset ] ] ]