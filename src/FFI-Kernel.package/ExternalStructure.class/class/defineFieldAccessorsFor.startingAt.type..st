field definition
defineFieldAccessorsFor: fieldName startingAt: byteOffset type: type
	"Define read/write accessors for the given field"
	| comment |
	(type isVoid and: [type isPointerType not]) ifTrue:[^self].
	comment := ('\	"This method was automatically generated. See ', self class name, '>>fields."\	') withCRs.
	self maybeCompileAccessor: fieldName, comment, (type readFieldAt: byteOffset)
		withSelector: fieldName asSymbol.
	self maybeCompileAccessor: fieldName,': anObject', comment, (type writeFieldAt: byteOffset with: 'anObject')
		withSelector: (fieldName, ':') asSymbol