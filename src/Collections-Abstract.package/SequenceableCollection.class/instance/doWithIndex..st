enumerating
doWithIndex: elementAndIndexBlock
	"Just like with:do: except that the iteration index supplies the second argument to the block. 
	#(11 22 13) doWithIndex: [ :each :i | Transcript show: (each * each + i) ;cr ].
	prints: 
		122
		486
		172

	Use the new version with consistent naming"
	^ self withIndexDo: elementAndIndexBlock