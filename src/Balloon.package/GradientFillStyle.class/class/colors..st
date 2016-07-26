instance creation
colors: colorArray
	"Create a gradient fill style from an array of equally spaced colors"
	^self ramp: (colorArray withIndexCollect:
		[:color :index| (index-1 asFloat / (colorArray size - 1 max: 1)) -> color]).