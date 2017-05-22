private
shouldCreateLocalBranchFromRemote 
	^ UIManager default
		confirm: 'You are trying to switch to a remote branch.' , String cr , 
			'Do you want to create a local branch for it?' , String cr ,
			'Note: if you do not create a local branch, you will be in a detached head and you won''t be abe to commit code!'
		label: 'Checkout remote branch'