tests - testing
testIfNilIfNotNil

	| object returnValue block |
	object := ProtoObject new.
	returnValue := Object new.
	self should: [ object ifNil: [ self error ] ifNotNil: [ self halt ] ] raise: Halt.
	self should: [ object ifNil: [ self error ] ifNotNil: [ :o | self halt ] ] raise: Halt.
	self assert: (object ifNil: [ false ] ifNotNil: [ :o | o == object ]).
	self assert: (object ifNil: [ nil ] ifNotNil: [ returnValue ]) == returnValue.
	self assert: (object ifNil: [ nil ] ifNotNil: [ :o | returnValue ]) == returnValue.
	"Now the same without inlining."
	block := [ self halt ].
	self should: [ object ifNil: [ self error ] ifNotNil: block ] raise: Halt.
	block := [ :o | self halt ].
	self should: [ object ifNil: [ self error ] ifNotNil: block ] raise: Halt.
	block := [ :o | o == object ].
	self assert: (object ifNil: [ false ] ifNotNil: block).
	block := [ returnValue ].
	self assert: (object ifNil: [ nil ] ifNotNil: block) = returnValue.
	block := [ :o | returnValue ].
	self assert: (object ifNil: [ nil ] ifNotNil: block) = returnValue