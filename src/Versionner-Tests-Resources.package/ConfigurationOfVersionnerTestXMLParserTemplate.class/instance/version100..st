versions
version100: spec
	<version: '1.0' imports: #('1.0-baseline')>
	spec
		for: #common
		do: [ 
			spec blessing: #release.
			spec
				package: 'VersionnerTestXML-Parser' with: 'XML-Parser-NorbertHartl.141';
				package: 'VersionnerTestXML-Tests-Parser' with: 'XML-Tests-Parser-JAA.14' 
				].
	
	