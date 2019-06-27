Extension { #name : #SpContactBookPresenter }

{ #category : #'*Spec2-Commander2-ContactBook-Extensions' }
SpContactBookPresenter class >> changePhoneCommandWith: presenter forRootGroup: aCmCommandsGroup [
	<extensionCommands>
	(aCmCommandsGroup / 'Context Menu' / 'Edition') "Inject an additional command in 'Edition' group."
		register: (SpChangePhoneCommand forSpec context: presenter)
]

{ #category : #'*Spec2-Commander2-ContactBook-Extensions' }
SpContactBookPresenter class >> extraCommandsWith: presenter forRootGroup: aCmCommandsGroup [
	<extensionCommands>
	aCmCommandsGroup / 'Context Menu'
		register: ((CmCommandGroup named: 'Extra') asSpecGroup
						basicDescription: 'Extra commands to help during development.';
						"Below is an example of reusing the same command for 2 different purposes."
						register: ((SpInspectContactCommand forSpec context: [ presenter selectedContact ]) "Here context is computed at the moment the command is executed."
											"The name and description can be adapted for its specific usage."
											basicName: 'Inspect contact';
											basicDescription: 'Open an inspector on the selected contact.';
											yourself);
						register: ((SpInspectContactCommand forSpec context: [ presenter contactBook ])
											basicName: 'Inspect contact book';
											basicDescription: 'Open an inspector on the contact book.';
											yourself);
						yourself)
]
