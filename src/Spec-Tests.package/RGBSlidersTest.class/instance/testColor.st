tests
testColor
	|random red green blue |
	random := Random new.
	red := random nextInt: 255.
	green :=  random nextInt: 255.
	blue :=  random nextInt: 255.
	
	testedInstance redSlider value: red.
	testedInstance greenSlider value:green.
	testedInstance blueSlider value:blue.
	
	self assert: (testedInstance color) = 
	(Color
		r: red
		g: green
		b: blue
		range: 255).