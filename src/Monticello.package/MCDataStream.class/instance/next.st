write and read
next
	"Answer the next object in the stream."
	| type selector anObject  internalObject |

	type := byteStream next.
	type ifNil: [
		byteStream close.	"clean up"
		byteStream position = 0 
			ifTrue: [self error: 'The file did not exist in this directory'] 
			ifFalse: [self error: 'Unexpected end of object file'].
		^ nil].
	type = 0 ifTrue: [
		byteStream close.	"clean up"
		self error: 'Expected start of object, but found 0'.
		^ nil].

	selector := self class readSelectors at: type 
		ifAbsent:[ 
				byteStream close.
				self error: 'Unrecognised type id. You should load the Monticello-OldDataStreamCompatibility package'
		].
	
	anObject := self perform: selector. "A method that recursively
		calls next (readArray, readInstance, objectAt:) must save &
		restore the current reference position."

		"After reading the externalObject, internalize it.
		 #readReference is a special case. Either:
		   (1) We actually have to read the object, recursively calling
			   next, which internalizes the object.
		   (2) We just read a reference to an object already read and
			   thus already interalized.
		 Either way, we must not re-internalize the object here."
	selector == #readReference ifTrue: [^ anObject].
	internalObject := anObject comeFullyUpOnReload: self.
	^ self maybeBeginReference: internalObject