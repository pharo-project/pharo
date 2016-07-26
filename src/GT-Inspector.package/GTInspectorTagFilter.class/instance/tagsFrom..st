accessing
tagsFrom: aMethod

	^ (aMethod pragmas select: [ :each | each key == self tagPragmaKey ])
		ifEmpty: [ defaultTag ifNil: [ {} ] ifNotNil: [ {defaultTag} ] ]
		ifNotEmpty: [ :currentPragmas | currentPragmas collect: [ :each | each argumentAt: 1 ] ]