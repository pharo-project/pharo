Extension { #name : 'ClassTest' }

{ #category : '*Shout-Tests' }
ClassTest >> testHasBindingThatBeginsWith [

	"Lookup in slots of the current class.
	This class defines #className"
	self assert: (self class hasBindingThatBeginsWith: 'classNam').
	
	"Pools are looked up, too"
	self assert: (PoolDefiner hasBindingThatBeginsWith: 'AnAu').
	self assert: (RootClassPoolUser hasBindingThatBeginsWith: 'AnAu').

	"Lookup in globals too"
	self assert: (self class hasBindingThatBeginsWith: 'Object').
]
