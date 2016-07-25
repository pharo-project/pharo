baselines
baseline10: spec
	<version: '1.0-baseline'>
	spec
		for: #common
		do: [ 
			spec
				blessing: #baseline;
				description: 'Copy of ConfigurationOfXMLParser used with Versionner tests.';
				author: 'ChristopheDemarey';
				timestamp: '27/03/2013';
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
