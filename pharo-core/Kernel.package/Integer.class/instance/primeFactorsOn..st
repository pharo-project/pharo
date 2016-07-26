accessing
primeFactorsOn: aStream
	"Recursively calculate the primefactors of myself and put the factors into the given stream"
	self = 1 
		ifTrue: [ ^ self ].
		
	self even ifTrue: [
		aStream nextPut: 2.
		^ (self / 2) primeFactorsOn: aStream ].
	
	3 to: self sqrtFloor by: 2 do: [ :each |
		self \\ each = 0 
			ifTrue: [ 
				aStream nextPut: each.
				^ (self / each) primeFactorsOn: aStream ]].
	
	aStream nextPut: self.