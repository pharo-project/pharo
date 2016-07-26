accessing
context

	^ context ifNil: [ context := self newContextForQuery: nil ]