private
evaluate: string
	| output |
	output := String streamContents: [ :out | 
		[ 	| compiler result |
			compiler := [ self class compiler ] 
								on: MessageNotUnderstood 
								do: [ Smalltalk at: #Compiler "for Pharo 2.0" ].		
			result := compiler evaluate: string. 
			out print: result; cr ]
			on: Error
			do: [ :exception |
				out print: exception; cr.
				exception signalerContext printDetails: out.
				exception signalerContext sender debugStack: 8 on: out ].
		out cr ].
	^ String lf join: output lines