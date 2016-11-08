initialize-release
initializeColorRegistry 
	| values|
	ColorRegistry := IdentityDictionary new.
	values := self defaultColors, self defaultColors2, self defaultColors3, self defaultColors4.
	1 to: values size by: #(name r g b) size do:[:index|
		|  colorName red green blue color |
		colorName := values at: index.
		red := values at: index + 1.
		green := values at: index + 2.
		blue := values at: index +3.
		color :=  self  r: red g: green b: blue .
		self registerColor: color named: colorName ].
	self registerColor: (self r: 0 g: 0 b: 0 alpha: 0.0) named: #transparent. 