settings
catalogSettingsOn: aBuilder
	<systemsettings>
	(aBuilder group: #gtspotter)
		parent: #gt;
		label: 'GT Spotter extensions';
		description: 'All extensions of GT Spotter';
		with:
				[ 
				| extensions groupedExtensions sortedAssociations |
				extensions := GTSpotter spotterExtendingMethods.
				groupedExtensions := (extensions groupedBy: #methodClass) associations.
				sortedAssociations := groupedExtensions asSortedCollection: [ :a1 :a2 | a1 key name < a2 key name ].
				sortedAssociations do: [ :association |
					(aBuilder group: association key name)
						parent: #gtspotter;
						label: 'Extensions defined on ', association key name;
						description: 'All extensions of ', association key name;
						with: [ 
							association value do:
								[ :each | 
									(aBuilder setting: (self internalKeyForMethod: each))
										target: self;
										label: (self spotterExtensionTitleOf: each);
										description: each sourceCode;
										default: true ] ] ]
				]
				