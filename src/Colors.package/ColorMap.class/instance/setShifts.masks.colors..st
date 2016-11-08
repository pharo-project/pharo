private
setShifts: shiftArray masks: maskArray colors: colorArray 
	shiftArray ifNotNil: [ shifts := shiftArray asIntegerArray ].
	maskArray ifNotNil: [ masks := maskArray asWordArray ].
	colorArray ifNotNil: [ colors := colorArray asWordArray ]