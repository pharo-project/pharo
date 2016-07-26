accessing
pragmas
	"Answer the raw messages comprising my pragmas."
	| pragmaStream |
	pragmaStream := WriteStream on: (Array new: self basicSize).
	1 to: self basicSize do: [:i |
		| propertyOrPragma "<Association|Message>" |
		(propertyOrPragma := self basicAt: i) isVariableBinding ifFalse:
			[pragmaStream nextPut: propertyOrPragma]].
	^pragmaStream contents