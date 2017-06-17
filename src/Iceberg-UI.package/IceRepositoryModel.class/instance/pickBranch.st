private
pickBranch 
	| branches |
	 
	branches := self repository branchesForCheckout.
	^ UIManager default
		chooseFrom: (branches collect: #name)
		values: branches
		message: 'Select the new branch you want to switch to:' 
		title: 'Checkout branch'