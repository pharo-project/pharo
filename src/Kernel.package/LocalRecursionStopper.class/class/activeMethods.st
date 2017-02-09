private
activeMethods
	^self value ifNil: [ self value: IdentitySet new. self value ]