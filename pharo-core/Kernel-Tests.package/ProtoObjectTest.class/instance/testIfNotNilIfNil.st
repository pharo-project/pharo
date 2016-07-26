tests - testing
testIfNotNilIfNil

	| object returnValue block |
	object := ProtoObject new.
	returnValue := Object new.
	self should: [ object ifNotNil: [ self halt ] ifNil: [ self error ]  ] raise: Halt.
	self should: [ object ifNotNil: [ :o | self halt ] ifNil: [ self error ] ] raise: Halt.
	self assert: (object ifNotNil: [ :o | o == object ] ifNil: [ false ]).
	self assert: (object ifNotNil: [ returnValue ] ifNil: [ false ]) == returnValue.
	self assert: (object ifNotNil: [ :o | returnValue ] ifNil: [ false ]) == returnValue.
	"Now the same without inlining."
	block := [ self halt ].
	self should: [ object ifNotNil: block ifNil: [ self error ]  ] raise: Halt.
	block := [ :o | self halt ].
	self should: [ object ifNotNil: block ifNil: [ self error ] ] raise: Halt.
	block := [ :o | o == object ].
	self assert: (object ifNotNil: block ifNil: [ false ]).
	block := [ returnValue ].
	self assert: (object ifNotNil: block ifNil: [ false ]) == returnValue.
	block := [ :o | returnValue ].
	self assert: (object ifNotNil: block ifNil: [ false ]) == returnValue