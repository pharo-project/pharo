converting
asValidSelector
	"e.g. '234znak ::x43 åå) :2' asValidSelector = #v234znak:x43:v2"
	
^(((
	$: join: (	
		(
			$: split: (
				self select: [ :char |
					(char charCode < 128) and: [ 
						char isAlphaNumeric or: [ 
							char = $:	
						]	
					]	
				]
			)
		)  
		select: [ :split | split isNotEmpty ]
		thenCollect: [ :nonEmptyString |
			nonEmptyString first isLetter
				ifTrue: [ nonEmptyString uncapitalized ]
				ifFalse: [ 'v' , nonEmptyString ]
		]
	)
) ifEmpty: [ 'v' ]), ((self isNotEmpty and: [ self last = $: ]) ifTrue: [ ':' ] ifFalse: [ #() ]) )asSymbol
