private
moveSourceToFront
	"Move the encoded contents of the receiver to the front so that we have enough space for decoding more data."
	(sourceStream == nil or:[sourceStream atEnd]) ifTrue:[^self].
	sourcePos > 10000 ifTrue:[
		source 
			replaceFrom: 1 
			to: source size - sourcePos
			with: source 
			startingAt: sourcePos + 1.
		source := sourceStream 
			next: sourcePos 
			into: source 
			startingAt: source size - sourcePos + 1.
		sourcePos := 0.
		sourceLimit := source size].