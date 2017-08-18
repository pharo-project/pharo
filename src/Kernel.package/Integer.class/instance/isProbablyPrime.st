testing
isProbablyPrime
	"See isProbablyPrimeWithK:andQ:randomIndex: for the algoritm description."
	
	| k q randomNumbers |
	self <= 1 ifTrue: [ ^false ].
	self even ifTrue: [ ^self = 2 ].
	"Factor self into (2 raisedTo: k) * q + 1, where q odd"
	q := self bitShift: -1.
	k := q lowBit.
	q := q bitShift: 1 - k.
	"Repeat the probabilistic until false (the probability of false negative is null) or until probability is very low."
	
	"Array of pregenerated 25 random numbers. "
	randomNumbers := #(0.816980664067427 0.9940209812456839 0.5106317962103671
		0.18859890764048273 0.7818407135931034 0.39687335928756434
		0.25054954609393587 0.9862212007801148 0.41972151138806785
		0.25944189925652084 0.44000080434605515 0.09351864414919105
		0.7678522154539136 0.292185133924794 0.7555458740124227
		0.4595045267881381 0.8925817282370206 0.621106479606175 
		0.9366027409846908 0.48226772969694237 0.47373301651036975
		0.030808489784043512 0.7982878004192784 0.8230616468112272
		0.19709795629470514).
	
	randomNumbers do: [:rnd | (self isProbablyPrimeWithK: k andQ: q randomIndex: rnd ) ifFalse: [ ^false ] ].
	"The probability of false positive after 25 iterations is less than (1/4 raisedTo: 25) < 1.0e-15"
	^true