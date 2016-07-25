utilities
switchBranchOf: anIceRepository
	| branches selection | 
	branches := anIceRepository localBranchNames.
	selection := (UIManager default 
		chooseFrom: branches
		message: 'Select the new branch you want to switch to:' 
		title: 'Checkout branch').

	selection > 0 ifTrue: [ anIceRepository checkoutBranch: (branches at: selection) ]
