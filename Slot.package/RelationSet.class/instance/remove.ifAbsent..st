removing
remove: oldObject ifAbsent: aBlock

	(set includes: oldObject)
		ifFalse: [ ^aBlock value ].
		
	slot remove: oldObject from: owner.
	^set remove: oldObject ifAbsent: aBlock