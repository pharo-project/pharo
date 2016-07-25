instance creation
default 
	| instance |
	instance := self new.
	instance parseOptions: OpalCompiler defaultOptions.
	AbstractCompiler compileWithErrors 
		ifTrue: [ instance parseOptions: #(+ optionParseErrors) ].
	^instance.