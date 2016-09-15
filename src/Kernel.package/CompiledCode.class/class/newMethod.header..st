instance creation
newMethod: numberOfBytes header: headerWord
	"Primitive. Answer an instance of me. The number of literals (and other 
	 information) is specified by the headerWord (see my class comment).
	 The first argument specifies the number of fields for bytecodes in the
	 method. Fail if either argument is not a SmallInteger, or if numberOfBytes
	 is negative, or if memory is low. Once the header of a method is set by
	 this primitive, it cannot be changed to change the number of literals.
	 Essential. See Object documentation whatIsAPrimitive."

	<primitive: 79 error: ec>
	ec == #'insufficient object memory' ifTrue:
		[^self handleFailingNewMethod: numberOfBytes header: headerWord].
	^self primitiveFailed