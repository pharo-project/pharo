tests - NaN behavior
testNaNCompare
	"IEEE 754 states that NaN cannot be ordered.
	As a consequence, every arithmetic comparison involving a NaN SHOULD return false.
	Except the is different test (~=).
	This test does verify this rule"
	
	| compareSelectors theNaN anotherNaN comparand brokenMethods warningMessage |
	self skip.
	compareSelectors := #(#< #<= #> #>= #=).
	theNaN := Float nan.
	anotherNaN := Float infinity - Float infinity.
	comparand := {1. 2.3. Float infinity. 2/3. 1.25s2. 2 raisedTo: 50}.
	comparand := comparand , (comparand collect: [:e | e negated]).
	comparand := comparand , {theNaN. anotherNaN}.

"do a first pass to collect all broken methods"
	brokenMethods := Set new.
	comparand do: [:comp |
		compareSelectors do: [:op |
			(theNaN perform: op with: comp) ifTrue: [brokenMethods add: (theNaN class lookupSelector: op)].
			(comp perform: op with: theNaN) ifTrue: [brokenMethods add: (comp class lookupSelector: op)]].
		(theNaN ~= comp) ifFalse: [brokenMethods add: (theNaN class lookupSelector: #~=)].
		(comp ~= theNaN) ifFalse: [brokenMethods add: (comp class lookupSelector: #~=)]].
	
"build a warning message to tell about all broken methods at once"
	warningMessage := String streamContents: [:s |
			s nextPutAll: 'According to IEEE 754 comparing with a NaN should always return false, except ~= that should return true.'; cr.
			s nextPutAll: 'All these methods failed to do so. They are either broken or call a broken one'.
			brokenMethods do: [:e | s cr; print: e methodClass; nextPutAll: '>>'; print: e selector]].
		
"Redo the tests so as to eventually open a debugger on one of the failures"
	brokenMethods := Set new.
	comparand do: [:comp2 |
		compareSelectors do: [:op2 |
			self deny: (theNaN perform: op2 with: comp2) description: warningMessage.
			self deny: (comp2 perform: op2 with: theNaN) description: warningMessage].
		self assert: (theNaN ~= comp2) description: warningMessage.
		self assert: (comp2 ~= theNaN) description: warningMessage].