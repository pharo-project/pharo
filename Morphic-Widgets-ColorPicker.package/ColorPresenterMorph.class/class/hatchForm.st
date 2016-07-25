graphics constants
hatchForm
	"Answer a form showing a grid hatch pattern."

	^HatchForm ifNil: [HatchForm := self newHatchForm]