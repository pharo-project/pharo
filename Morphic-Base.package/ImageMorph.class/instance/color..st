accessing
color: aColor
	"Set the color.
	Change to a ColorForm here if depth 1."
	
	super color: aColor.
    (image depth = 1 and: [aColor isColor]) ifTrue: [
		image isColorForm ifFalse: [
			image := ColorForm mappingWhiteToTransparentFrom: image].
			image colors: {Color transparent. aColor}.
			self changed]