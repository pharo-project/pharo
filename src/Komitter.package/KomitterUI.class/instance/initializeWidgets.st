initialization
initializeWidgets

	tree := self newTree.
	diff := self instantiate: DiffPresenter.
	message := self newText.
	author := self newTextInput.
	authorName := self newLabel.
	commitMessage := self newLabel.
	remotes := self newDropList.
	remoteLabel := self newLabel.
	checkCommit := self newCheckBox.
	previousMessages := self newButton.
	manageRemotes := self newButton.
	
	tree 
		removeOnlyLastSelected: true;
		autoMultiSelection: true;
		columns: { TreeColumnPresenter new
			displayBlock: [:node | node label ] }.
			
	diff
		showOptions: false;
		leftText: 'No selection for diff'.

	authorName label: 'Author:'.
	commitMessage label: 'Commit message:'.
	remoteLabel label: 'Remote:'.
	
	checkCommit
		label: 'Check Lint rules (NOT YET WORKING)';
		labelClickable: true;
		state: false;
		enabled: false.	
		
	author 
		text: Author fullNamePerSe;
		autoAccept: true.
		
	previousMessages
		icon: self historyIcon;
		action: [ self chooseFromPreviousMessages ];
		bindKeyCombination: Character space asKeyCombination 
		toAction: [ previousMessages action value ];
		help: 'Previous messages'.
		
	message
		bindKeyCombination: Character tab asKeyCombination
		toAction: [ self giveFocusToNextFrom: message ];
		bindKeyCombination: Character tab shift
		toAction: [ self giveFocusToPreviousFrom: message ];
		wantsVisualFeedback: false.
	
	manageRemotes
		icon: self manageRemotesIcon;
		action: [ self openRemoteManager ];
		help: 'Open the remotes manager'.
	
	self whenBuiltDo: [ 
		checkCommit labelOnLeft.
		diff showOnlySource: true.
		message selectAll.
		self updateRemotes ].