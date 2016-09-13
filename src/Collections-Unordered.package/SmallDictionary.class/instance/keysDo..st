enumerating
keysDo: aBlock 
	1 to: size do: [:i | aBlock value: (keys at: i)]