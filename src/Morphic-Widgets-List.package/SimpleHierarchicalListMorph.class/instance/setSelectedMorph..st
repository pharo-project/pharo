selection
setSelectedMorph: aMorph

	model 
		perform: (setSelectionSelector ifNil: [^self]) 
		with: aMorph complexContents	"leave last wrapper in place"

 