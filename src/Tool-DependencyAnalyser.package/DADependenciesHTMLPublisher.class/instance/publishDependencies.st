publishing
publishDependencies
	stream
		<<
			'				<table id="example" class="display" cellspacing="0" width="100%">
					<thead>
						<tr>
							<th>Package</th>
							<th>Dependencies</th>
							<th>Dependants</th>
						</tr>
					</thead>
					<tbody>';
		lf.
	report
		dependenciesAndKnownDependantsDo:
			[ :packageName :dependencies :dependants | self
				publish: packageName
				dependencies: dependencies
				dependants: dependants ].
	self publishPackagesToRemoveDependants.
	stream
		<<
			'					</tbody>
				</table>';
		lf