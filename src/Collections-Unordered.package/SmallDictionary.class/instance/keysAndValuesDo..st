enumerating
keysAndValuesDo: aBlock
	1 to: size do: [:i | aBlock value: (keys at: i) value: (values at: i)]