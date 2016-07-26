accessing
color
	^ color
		ifNil: [color := Color colorFrom: (SHPreferences customStyleTable
							detect: [:e | self tokens first = e first]) second]