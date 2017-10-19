testing
testPrimeFactors
	"[ self run: #testPrimeFactors ] bench."
	
	| cache data |
	cache := self newCache.
	cache maximumWeight: 512.
	cache factory: [ :key | key ].
	data := Array streamContents: [ :out |
		1 to: 4096 do: [ :each |
			each primeFactorsOn: out.
			out nextPut: each ] ].
	data := data collect: [ :each | each asWords ].
	data do: [ :each | cache at: each ].
	self assert: cache totalWeight equals: 512.
	self assert: cache hitRatio > (7/10).
	cache validateInvariantWith: self