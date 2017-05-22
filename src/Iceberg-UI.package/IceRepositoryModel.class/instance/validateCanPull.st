private
validateCanPull
	self repository isModified
		ifTrue: [ 
			^ IceTool uiManager
				confirm:
					'Your repository has uncommited changes. Merging incoming commits will change
your current working copy and your current state will not be recoverable. 
We recommend that you commit first and then pull incoming changes again.'
				label: 'You might loose your current changes!'
				trueChoice: 'Proceed anyway'
				falseChoice: 'Cancel'
				cancelChoice: nil
				default: false ].
	^ true