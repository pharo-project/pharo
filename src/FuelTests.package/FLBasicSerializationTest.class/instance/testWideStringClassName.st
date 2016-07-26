tests-strings
testWideStringClassName
	| class className |
	className := 'Foox' asWideString
		at: 4 put: (Character value: 265);
		yourself.
	class := Class new setName: className; yourself.
	[
		Smalltalk at: class name put: class.
		self
			shouldnt: [ FLSerializer serializeToByteArray: (Smalltalk at: class name) ]
			raise: Error
	] ensure:[ Smalltalk removeKey: class name ]