enumerating
withIndexDo: elementAndIndexBlock 
	"Just like with:do: except that the iteration index supplies the second argument to the block. "
	"#(11 22 13) withIndexDo: [ :each :i | Transcript show: (each * each + i) ;cr ].
		122
		486
		172"
		
	1 to: self size do:
		[:index |
		elementAndIndexBlock
			value: (self at: index)
			value: index]