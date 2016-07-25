private
generateMethodFromSpec: functionSpec
	| ir |
	ir := IRBuilder buildIR: [ :builder | 
		self generateFFICallout: builder spec: functionSpec ].
	^ ir generate: self method trailer