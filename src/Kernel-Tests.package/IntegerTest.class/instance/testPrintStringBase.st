tests - printing
testPrintStringBase
	
	2 to: 32 do: [:b |
		1 to: 1000//b do: [:n |
			| bRaisedToN |
			bRaisedToN := b raisedTo: n.
			self assert: (bRaisedToN - 1 printStringBase: b) = (String new: n withAll: (Character digitValue: b-1)).
			self assert: (bRaisedToN printStringBase: b) = ('1' , (String new: n withAll: $0)).
			
			self assert: (bRaisedToN negated + 1 printStringBase: b) = ('-' , (String new: n withAll: (Character digitValue: b-1))).
			self assert: (bRaisedToN negated printStringBase: b) = ('-1' , (String new: n withAll: $0))]].
