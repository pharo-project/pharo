enumerating - values
valuesDo: aBlock 
	"Evaluate aBlock for each of the receiver's values."

1 to: size do: [:i | aBlock value: (values at: i)]