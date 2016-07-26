tests - testing
testIfNotNil

	| object returnValue block |
	object := ProtoObject new.
	returnValue := Object new.
	self should: [ object ifNotNil: [ self halt ] ] raise: Halt.
	self should: [ object ifNotNil: [ :o | self halt ] ] raise: Halt.
	self assert: (object ifNotNil: [ :o | o == object ]).
	self assert: (object ifNotNil: [ returnValue ]) == returnValue.
	self assert: (object ifNotNil: [ :o | returnValue ]) == returnValue.	
	"Now the same without inlining."
	block := [ self halt ].
	self should: [ object ifNotNil: block ] raise: Halt.
	block := [ :o | self halt ].
	self should: [ object ifNotNil: block ] raise: Halt.
	block := [ :o | o == object ].
	self assert: (object ifNotNil: block).
	block := [ returnValue ].
	self assert: (object ifNotNil: block) = returnValue.
	block := [ :o | returnValue ].
	self assert: (object ifNotNil: block) = returnValue