expected results
expectedTestResolveSymbolicVersionInResult
	^ 'testVersion: spec
	<version: ''3.0'' imports: #(''1.0-baseline'')>
	spec
		for: #common
		do:
			[ spec blessing: #release.
			spec
				package: ''VersionnerTestXML-Parser''
					with: ''VersionnerTestXML-Parser-StephaneDucasse.142'';
				package: ''VersionnerTestXML-Tests-Parser''
					with:
					''VersionnerTestXML-Tests-Parser-StephaneDucasse.15'' ]'