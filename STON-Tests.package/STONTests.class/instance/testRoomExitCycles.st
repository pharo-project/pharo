tests
testRoomExitCycles
	| model room1 room2 exit1 exit2 ston object |
	(room1 := STONTestMap new) at: #name put: 'Room 1'.
	(room2 := STONTestMap new) at: #name put: 'Room 2'.
	(exit1 := STONTestMap new)
		at: #name put: 'Exit 1';
		at: #origin put: room1;
		at: #destination put: room2.
	(exit2 := STONTestMap new)
		at: #name put: 'Exit 2';
		at: #origin put: room2;
		at: #destination put: room1.
	room1 at: #exit put: exit1.
	room2 at: #exit put: exit2.
	model := Array with: room1 with: room2.
	ston := STON toString: model.
	object := STON fromString: ston.
	"We can't just compare because this is a recursive datastructure"
	self assert: (object first at: #name) equals: 'Room 1'.
	self assert: (object second at: #name) equals: 'Room 2'.
	self assert: ((object first at: #exit) at: #name) equals: 'Exit 1'.
	self assert: ((object second at: #exit) at: #name) equals: 'Exit 2'.
	self assert: ((object first at: #exit) at: #origin) == object first.
	self assert: ((object first at: #exit) at: #destination) == object second.
	self assert: ((object second at: #exit) at: #origin) == object second.
	self assert: ((object second at: #exit) at: #destination) == object first.
	"Try writing again the parse model" 
	self assert: (STON toString: object) equals: ston