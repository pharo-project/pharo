actions
promptForPackages
	| choosenpackages |
	choosenpackages := (RPackageOrganizer default packages
				reject: [:package | (package packageName beginsWith: 'Kernel')
						or: [(package packageName beginsWith: 'Collections')
								or: [(package packageName beginsWith: 'Exceptions')
										or: [(package packageName beginsWith: 'SUnit')
												or: [(package packageName beginsWith: 'System')
														or: [package packageName includesSubstring: 'Test' caseSensitive: false]]]]]])
				sort: [:a :b | a packageName < b packageName].
	choosenpackages := Array
				with: (UIManager default
						chooseFrom: (choosenpackages
								collect: [:package | package packageName])
						values: choosenpackages
						title: 'Select Package').
	^ choosenpackages