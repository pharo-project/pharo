pixel mapping
mappingTo: aColorMap 
	"Compute a new color map through the receiver and aColorMap.
	Both maps are assumed to be mappings into canonical ARGB space"
	| fixedMap |
	self = aColorMap ifTrue: [ ^ nil ].	"No mapping needed"
	aColorMap isIndexed ifTrue: [ ^ nil ].	"We can't compute mappings to an indexed map yet"
	fixedMap := self class 
		mappingFrom: self rgbaBitMasks
		to: aColorMap rgbaBitMasks.
	self isIndexed ifFalse: [ ^ fixedMap ].
	"If the receiver is indexed then we need to map the colors as well"
	self flag: #untested.
	^ ColorMap 
		shifts: fixedMap shifts
		masks: fixedMap masks
		colors: (colors collect: [ :pv | aColorMap pixelMap: pv ])