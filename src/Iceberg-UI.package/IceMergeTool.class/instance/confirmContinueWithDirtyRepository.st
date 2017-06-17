utilities
confirmContinueWithDirtyRepository
	^ self uiManager
		confirm:
'Your repository has uncommited changes. Merge will change your current 
working copy and your current state will not be recoverable. 
We recommend that you commit first and then pull incoming changes again.'
		label: 'You might loose your current changes!'
		trueChoice: 'Proceed anyway'
		falseChoice: 'Cancel'
		cancelChoice: nil
		default: false