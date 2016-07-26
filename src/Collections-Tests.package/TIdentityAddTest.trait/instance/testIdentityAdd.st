tests - identity adding
testIdentityAdd
	| added oldSize |
	oldSize := self collection size.
	self collection add: self element.
	self deny: (self collection includes: self equalNotIdenticalElement).
	
	added := self collection add: self equalNotIdenticalElement.
	self assert: added == self equalNotIdenticalElement.
	self assert: (self collection includes: self equalNotIdenticalElement)