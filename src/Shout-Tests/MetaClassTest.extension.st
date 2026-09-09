Extension { #name : 'MetaClassTest' }

{ #category : '*Shout-Tests' }
MetaClassTest >> testHasBindingThatBeginsWith [
	self assert: (SmalltalkImage class hasBindingThatBeginsWith: 'Compiler').
	self assert: (SmalltalkImage class hasBindingThatBeginsWith: 'Object').

	"Pools are looked up, too"
	self assert: (PoolDefiner class hasBindingThatBeginsWith: 'AnAu')
]
