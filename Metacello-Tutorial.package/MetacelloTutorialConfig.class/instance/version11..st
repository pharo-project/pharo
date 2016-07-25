versions
version11: spec 
	<version: '1.1' imports: #('1.0-baseline')>
	
	spec for: #common do: [
		spec blessing: #development.
		spec description: 'Example of a complete version specification'.
		spec author: 'dkh'.
		spec timestamp: '10/12/2009 09:26'.
		spec 
			package: 'Example-Core' with: 'Example-Core-anon.15';
			package: 'Example-Tests' with: 'Example-Tests-anon.4';
			package: 'Example-AddOn' with: 'Example-AddOn-anon.1';
			package: 'Example-AddOnTests' with: 'Example-AddOnTests-anon.1' ].
