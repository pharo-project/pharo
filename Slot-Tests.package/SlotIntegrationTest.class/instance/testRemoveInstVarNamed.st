tests
testRemoveInstVarNamed

	| answer |

	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			slots: #(a) 
		].

	answer := 
		aClass 
			slotNamed: 'a' 
			ifFound: [ #found ] 
			ifNone: [ #none ].
	self assert: answer equals: #found.

	answer := 
		aClass 
			slotNamed: 'b' 
			ifFound: [ #found ] 
			ifNone: [ #none ].
	self assert: answer equals: #none.
