versions
version011: spec
	<version: '0.1.1' imports: #('0.1.0-baseline')>

	spec for: #common do: [ 
		spec
			package: 'ShoreLine-Report-Core' with: 'ShoreLine-Report-Core-TommasoDalSasso.6';
			package: 'ShoreLine-Report-UI' with: 'ShoreLine-Report-UI-TommasoDalSasso.10';
			package: 'ShoreLine-Report-Settings' with: 'ShoreLine-Report-Settings-TommasoDalSasso.2' ]