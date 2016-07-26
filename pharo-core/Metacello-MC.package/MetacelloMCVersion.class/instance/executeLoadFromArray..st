private
executeLoadFromArray: anArray

	| loader mcLoader |
	loader := MetacelloMCVersionSpecLoader on: self spec.
	loader required: anArray.
	loaderPolicy notNil ifTrue: [ loader loaderPolicy: loaderPolicy ].
	^loader load