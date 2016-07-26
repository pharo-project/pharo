adding
addAllBricks: aCollection

	aCollection do: [ :each | self addBrickBack: each ]