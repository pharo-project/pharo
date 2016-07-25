private
privateNextValue
	"Answer a random Float in the interval [0 to 1)."

	^ (seed := self privateNextSeed) / m