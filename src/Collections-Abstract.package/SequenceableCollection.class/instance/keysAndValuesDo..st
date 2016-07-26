enumerating
keysAndValuesDo: aBlock 
	"Enumerate the receiver with all the keys (aka indices) and values."

	1 to: self size do: [:index | aBlock value: index value: (self at: index)]