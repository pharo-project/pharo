enumerating
select: aBlock 
	"Reimplemennt #select: for speedup on linked lists. 
	The super implemention accesses the linkes by index, thus causing an O(n^2)"
	
	| newCollection |
	newCollection := self class new.
	self do: [ :each | 
		(aBlock value: each) 
			ifTrue: [ newCollection add: each ]].
	^newCollection