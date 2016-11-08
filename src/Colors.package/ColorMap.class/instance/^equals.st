comparing
= aColorMap
	"Return true if the receiver is equal to aColorMap"
	self species == aColorMap species ifFalse:[^false].
	self isIndexed == aColorMap isIndexed ifFalse:[^false].
	^self colors = aColorMap colors and:[
		self shifts = aColorMap shifts and:[
			self masks = aColorMap masks]]