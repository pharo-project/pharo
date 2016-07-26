baselines
baseline20: spec
	<version: '2.0-baseline'>
	spec
		for: #common
		do: [ 
			spec
				blessing: #baseline;
				description: 'New baseline to test use of baselines in numbered versions';
				author: 'ChristopheDemarey';
				timestamp: '25/02/2014';
				repository: 'http://www.smalltalkhub.com/mc/PharoExtras/XMLParser/main'.

			spec project: 'XMLWriter'
					with: [ spec 
								className: 'ConfigurationOfVersionnerTestXMLWriter';
								versionString: #stable;
								file: 'Versionner-Tests-Resources';
								repository: 'http://www.smalltalkhub.com/mc/demarey/Versionner/main'.].

			spec project: 'BitmapCharacterSet'
					with: [ spec 
								className: 'ConfigurationOfVersionnerTestBitmapCharacterSet';
								versionString: #stable;
								file: 'Versionner-Tests-Resources';
								repository: 'http://www.smalltalkhub.com/mc/demarey/Versionner/main'.].
			
			spec 
				package: 'VersionnerTestXML-Parser' 
				with: [spec 
						requires: #('BitmapCharacterSet' 'XMLWriter')].	
			spec package: 'VersionnerTestXML-Tests-Parser'
					with: [ spec requires: 'VersionnerTestXML-Parser' ].
			
			spec
				group: 'default' with: #('Core' 'Tests');
				"default by default is equivalent to loading 'ALL' the packages still we added explicitly Tests to be loaded"
				group: 'Core' with: #('VersionnerTestXML-Parser');
				group: 'Tests' with: #('VersionnerTestXML-Tests-Parser') ].
