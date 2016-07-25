tests
testAsArray

	| item set |

	set := WeakSet new.
	
	self assert: set asArray = #().
	
	item := Object new.
	set add: item.
	
	self assert: set asArray = { item }.
	
	item := nil.
	Smalltalk garbageCollect.
	
	self assert: set asArray = #()
