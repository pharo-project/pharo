selection
selectedMethodIndex

	^ self getMethods identityIndexOf: self selectedMethod ifAbsent: [ 0 ].