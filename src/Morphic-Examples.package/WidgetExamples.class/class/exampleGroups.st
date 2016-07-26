examples
exampleGroups
	"Open a window with examples of each of the group type morphs."

	"self exampleGroups"

	| dialog builder |
	builder := self exampleBuilder.
	dialog := (builder newPluggableDialogWindow: 'Example groups')
		useDefaultOKButton.
	dialog
		contentMorph:
			((dialog
				newRow:
					{(dialog
						newLabelGroup:
							{('Plain Groupbox'
								->
									(dialog
										newGroupboxForAll:
											{(dialog
												newButtonFor: nil
												action: nil
												label: 'A Button'
												help: 'This is a button').
											(dialog
												newButtonFor: nil
												action: nil
												label: 'A Button'
												help: 'This is a button')})).
							('Groupbox'
								->
									(dialog
										newGroupbox: 'A groupbox'
										forAll:
											{(dialog
												newButtonFor: nil
												action: nil
												label: 'A Button'
												help: 'This is a button').
											(dialog
												newButtonFor: nil
												action: nil
												label: 'A Button'
												help: 'This is a button')}))}).
					(dialog
						newLabelGroup:
							{('Tab Group'
								->
									(dialog
										newTabGroup:
											{((dialog
												newRow:
													{(dialog newImage: (Smalltalk ui icons iconNamed: #smallLeftFlushIcon)).
													(dialog newLabel: 'Page 1')})
												->
													(dialog
														newGroupbox: 'A groupbox'
														forAll:
															{(dialog
																newButtonFor: nil
																action: nil
																label: 'A Button'
																help: 'This is a button').
															(dialog
																newButtonFor: nil
																action: nil
																label: 'A Button'
																help: 'This is a button')})).
											('Page 2'
												->
													(dialog
														newCheckboxFor: (ValueHolder new contents: true)
														getSelected: #contents
														setSelected: #contents:
														label: 'A Checkbox'
														help: 'This is a checkbox'))}))})})
				vResizing: #spaceFill);
		model: nil.
	builder openModal: dialog