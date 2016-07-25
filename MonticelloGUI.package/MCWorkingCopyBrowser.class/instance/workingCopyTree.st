morphic ui
workingCopyTree
	| workingCopies |
	workingCopies := self workingCopies.
	
	"filter the working copy list if there is a serach string"
	workingCopyPattern ifNotEmpty: [ 
		workingCopies := workingCopies select: [ :each| each package name asLowercase includesSubstring: workingCopyPattern ]].
			
	^ workingCopies collect: [:each| MCDependentsWrapper with: each model: self].