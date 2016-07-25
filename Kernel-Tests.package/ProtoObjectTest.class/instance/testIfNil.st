tests - testing
testIfNil
	| object block |
	object := ProtoObject new.
	object ifNil: [ self halt ].
	self assert: (object ifNil: [ nil ]) == object.	"Now the same without inlining."
	block := [ self halt ].
	object ifNil: block.
	block := [ nil ].
	self assert: (object ifNil: block) == object