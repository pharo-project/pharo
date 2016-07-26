write and read
objectAt: anInteger
	"PRIVATE -- Read & return the object at a given stream position.  08:18 tk  anInteger is a relative file position. "
	| savedPosn anObject refPosn |

	savedPosn := byteStream position.	"absolute"
	refPosn := self getCurrentReference.	"relative position"

	byteStream position: anInteger + basePos.	"was relative"
	anObject := self next.

	self setCurrentReference: refPosn.	"relative position"
	byteStream position: savedPosn.		"absolute"
	^ anObject