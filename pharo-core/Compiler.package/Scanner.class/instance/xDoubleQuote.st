multi-character scans
xDoubleQuote
	"Collect a comment."
	| aStream |

	aStream := (String new: 200) writeStream.
	self step.
	[hereChar == $"]
		whileFalse:
			[hereChar == DoItCharacter
				ifTrue: [^self offEnd: 'Unmatched comment quote'].
			aStream nextPut: self step.].
	self step.
	(currentComment ifNil: [ 
		currentComment := OrderedCollection new ])
			add: aStream contents.
	self scanToken