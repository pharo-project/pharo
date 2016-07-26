baselines
baseline100: spec 
	<version: '1.0-baseline'>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec repository: 'http://smalltalkhub.com/mc/PharoExtras/BitmapCharacterSet/main'.	
		spec description: 'Imported from SqueakSource'.
		spec package: 'Collections-BitmapCharacterSet'.
		spec 
			package: 'Collections-Tests-BitmapCharacterSet' 
			with: [spec requires: 'Collections-BitmapCharacterSet'].
			
		spec
			group: 'default' with: #('Core');
			group: 'Core' with: #('Collections-BitmapCharacterSet');
			group: 'Tests' with: #('Collections-Tests-BitmapCharacterSet'). ].