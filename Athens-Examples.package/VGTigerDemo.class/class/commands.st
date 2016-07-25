private
commands
" VGTigerDemo commands 
"
	| pt str |
	
	pt := 1.
	
	str := String new: self tigerCommandCount.
	
	"merge everything into a single string"
	self tigerCommands do: [:sub |

		0 to: sub size-1 do: [:i |
			str at: pt+i put: (sub at: i+1).
			].
		pt := pt + sub size.	
	].

	^ str