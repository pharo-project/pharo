converting
removeAttributesThat: removalBlock replaceAttributesThat: replaceBlock by: convertBlock
	"Enumerate all attributes in the receiver. Remove those passing removalBlock and replace those passing replaceBlock after converting it through convertBlock"
	| added removed |
	"Deliberately optimized for the no-op default."
	added := removed := nil.
	runs withStartStopAndValueDo: [ :start :stop :attribs | 
		attribs do: [ :attrib | | new |
			(removalBlock value: attrib) ifTrue:[
				removed ifNil:[removed := Array new writeStream].
				removed nextPut: {start. stop. attrib}.
			] ifFalse:[
				(replaceBlock value: attrib) ifTrue:[
					removed ifNil:[removed := Array new writeStream].
					removed nextPut: {start. stop. attrib}.
					new := convertBlock value: attrib.
					added ifNil:[added := Array new writeStream].
					added nextPut: {start. stop. new}.
				].
			].
		].
	].
	(added isNil and:[removed isNil]) ifTrue:[^self].
	"otherwise do the real work"
	removed ifNotNil:[removed contents do:[:spec|
		self removeAttribute: spec last from: spec first to: spec second]].
	added ifNotNil:[added contents do:[:spec|
		self addAttribute: spec last from: spec first to: spec second]].