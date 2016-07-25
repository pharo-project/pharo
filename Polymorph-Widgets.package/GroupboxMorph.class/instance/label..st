accessing
label: aString
	"Set the contents of the label morph."

	aString ifNil: [
		self roundedCorners: #(1 2 3 4).
		self labelMorph delete. ^self].
	self roundedCorners: #(2 3 4).
	self labelMorph owner ifNil: [
		self addMorph: self labelMorph].
	self labelMorph contents: aString