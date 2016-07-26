accessing
from: startIndex to: endIndex put: anObject
	"Put anObject in all indexes between startIndex 
	and endIndex. Very fast. Faster than to:do: for
	more than 26 positions. Answer anObject"

	| written toWrite thisWrite |

	startIndex > endIndex ifTrue: [^self].
	self at: startIndex put: anObject.
	written := 1.
	toWrite := endIndex - startIndex + 1.
	[written < toWrite] whileTrue:
		[
			thisWrite := written min: toWrite - written.
			self 
				replaceFrom: startIndex + written
				to: startIndex + written + thisWrite - 1
				with: self startingAt: startIndex.
			written := written + thisWrite
		].
	^anObject