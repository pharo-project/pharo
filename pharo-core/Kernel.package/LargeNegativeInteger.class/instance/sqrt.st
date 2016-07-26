mathematical functions
sqrt
	"Answer the square root of the receiver."
	^ DomainError signal: 'sqrt undefined for number less than zero.'