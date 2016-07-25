private-type-guessing
nonAbstractTypeOf: aRange
	| cls |
	cls := aRange asType.
	cls hasAbstractMethods
		ifFalse: [ ^ cls ].
	^ [(cls readFromString: (aRange textIn: source)) class] on: Error do: [ cls ]