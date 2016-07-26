versions
version13: spec 
	<version: '1.3' imports: #('1.3-baseline')>
	
	spec for: #common do: [
		spec blessing: #development.
		spec description: 'Add in doits for Example-Core'.
		spec author: 'dkh'.
		spec timestamp: '10/13/2009 14:27'.
		spec 
			package: 'Example-Core' with: 'Example-Core-anon.17';
			package: 'Example-Tests' with: 'Example-Tests-anon.6';
			package: 'Example-AddOn' with: 'Example-AddOn-anon.1';
			package: 'Example-AddOnTests' with: 'Example-AddOnTests-anon.1' ].
	spec for: #gemstone do: [
		spec package: 'Example-Platform' with: 'Example-Platform.gemstone-dkh.4'.].
	spec for: #pharo do: [
		spec package: 'Example-Platform' with: 'Example-Platform.pharo-dkh.7'.].
	spec for: #squeak do: [
		spec package: 'Example-Platform' with: 'Example-Platform.squeak-dkh.3'.].