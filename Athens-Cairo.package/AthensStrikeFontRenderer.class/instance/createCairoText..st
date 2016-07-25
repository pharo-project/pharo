private
createCairoText: aString
	| fdesc at |
	at := AthensCairoText new.
	at text: aString.
	fdesc := font asAthensFontDescription.
	at readFrom: fdesc.
	^ at