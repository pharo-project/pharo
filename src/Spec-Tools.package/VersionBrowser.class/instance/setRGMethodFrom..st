private
setRGMethodFrom: aMethod
"asHistorical, because active RGMethods don't always have the source pointer attached"
	rgMethod := aMethod asRingDefinition asHistorical.