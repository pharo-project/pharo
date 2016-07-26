copying
clone
	"Answer a shallow copy of the receiver."
	<primitive: 148 error: ec>
	| class newObject |
	ec == #'insufficient object memory' ifFalse:
		[^self primitiveFailed].
	"If the primitive fails due to insufficient memory, instantiate via basicNew: to invoke
	 the garbage collector before retrying, and use copyFrom: to copy state."
	newObject := (class := self class) isVariable
					ifTrue: 
						[class isCompiledMethodClass
							ifTrue:
								[class newMethod: self basicSize - self initialPC + 1 header: self header]
							ifFalse:
								[class basicNew: self basicSize]]
					ifFalse:
						[class basicNew].
	^newObject copyFrom: self