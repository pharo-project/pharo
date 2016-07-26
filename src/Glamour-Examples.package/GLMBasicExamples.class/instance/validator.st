browsers
validator
	<glmBrowser: 'Validator' input: '42'>
	"self new validator openOn: 42"

	| finder validator |
	finder := GLMFinder new.
	validator := GLMValidator new.
	validator validator: 'Ok' act: [:x | ].
	validator show: [:a | a list display: [:x | 1 to: x ]].
	finder show: [:a | a custom: validator].
	^ finder