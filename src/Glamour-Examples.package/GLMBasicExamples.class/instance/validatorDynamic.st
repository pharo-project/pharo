browsers
validatorDynamic
	<glmBrowser: 'Dynamic validator' input: '42'>
	"self new validatorDynamic openOn: 42"

	| finder validator |
	finder := GLMFinder new.
	validator := GLMValidator new.
	validator validator: 'Ok' act: [:x | ].
	validator show: [:a | 
		a dynamic display: [:x |
			GLMListPresentation new display: [:y | 1 to: y]
			] ].
	finder show: [:a | a custom: validator].
	^ finder