testing-enquiries
testClassMethodsTakePrecedenceOverTraitsMethods
	| keys |
	keys := Set new.
	self t4 methodDict bindingsDo: [:each | keys add: each key].
	self assert: keys size = 6.
	self 
		assert: (keys includesAll: #(
						#m12
						#m13
						#m13
						#m21
						#m22
						#m11
						#m42
					)).
	self assert: (self t4 methodDict at: #m11) sourceCode = 'm11 ^41'