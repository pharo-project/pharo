accessing
loader

	loader == nil 
		ifTrue: [
			loader := self versionSpec loader copy.
			loader spec: self.
			loaderPolicy notNil ifTrue: [ loader loaderPolicy: loaderPolicy] ].
	^loader