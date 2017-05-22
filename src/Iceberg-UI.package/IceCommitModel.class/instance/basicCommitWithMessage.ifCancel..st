private
basicCommitWithMessage: text ifCancel: cancelBlock
	UIManager default informUserDuring: [ :bar | 
		bar label: ('Commiting to {1}' format: {self repository printString}).
		[
			[ self repository 
				commitPackages: self model changedPackages
				withMessage: text asString ]
			on: IceGitUsernameOrEmailNotFound do: [ :e |
				self configGitUsernameAndEmailOnCancel: cancelBlock.
				e retry ].
			self message text: ''.
			UIManager default inform: ('Commited with message ', text) ]
		on: NothingToCommitException 
		do: [ :e | UIManager default inform: 'There is nothing to commit.' ] ]
