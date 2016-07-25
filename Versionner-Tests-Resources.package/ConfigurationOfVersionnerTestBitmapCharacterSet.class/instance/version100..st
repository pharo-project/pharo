versions
version100: spec 
	<version: '1.0' imports: #('1.0-baseline') >
	
	spec for: #common do: [
		spec blessing: #release.
		spec description: 'Latest versions from Squeaksource'.
		
		spec package: 'Collections-BitmapCharacterSet' with: ' Collections-BitmapCharacterSet-JAA.5'.
		spec package: 'Collections-Tests-BitmapCharacterSet' with: 'Collections-Tests-BitmapCharacterSet-JAA.5'
		]
	

