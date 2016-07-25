accessing
parseOptions: optionsArray
	"parse an array, which is a sequence of options in a form of: 
	
	#( + option1 option2 option3: param3  - option5 -option6: ... )
	
	each time the #+ is seen, the options which follow it will be subject for inclusion
	and, correspondingly, if #- seen, then they will be excluded	.

	By default, (if none of #+ or #- specified initially), all options are subject for inclusion.
	
	If in inclusion mode, non-unary options will put the following element as its unary symbols counterparts value, rather than true.
	"

	| include option rs |
	include := true.
	rs := optionsArray readStream.
	[(option := rs next) isNil]
		whileFalse: [ 
			option
				caseOf:
					{(#+ -> [ include := true ]).
					(#- -> [ include := false ])}
				otherwise: [ 
					(option beginsWith: 'opt') not
						ifTrue: [ ^ self error: 'Invalid option name ' , option , ', must begin with ''opt''' ].
					option numArgs > 1
						ifTrue: [ ^ self error: 'Does not support setting options with more than one parameter' ].
					options at: (option allButLast: option numArgs) asSymbol put: (include and: [ option isUnary or: [ rs next ] ]) ] ]