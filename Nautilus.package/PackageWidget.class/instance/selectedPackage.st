protocol
selectedPackage

	^ self selectedPackages ifEmpty: [ nil ] ifNotEmpty: [ :col | col first ]