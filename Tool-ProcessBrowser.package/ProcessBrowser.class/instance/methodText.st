accessing
methodText
	^ methodText ifNil: [methodText := selectedContext
						ifNil: ['']
						ifNotNil: [| pcRange | 
							methodText := [ selectedContext sourceCode ]
								ifError: [ 'error getting method text' ].
							pcRange := self pcRange.
							methodText asText
								addAttribute: TextColor red
								from: pcRange first
								to: pcRange last;
								addAttribute: TextEmphasis bold
								from: pcRange first
								to: pcRange last]]