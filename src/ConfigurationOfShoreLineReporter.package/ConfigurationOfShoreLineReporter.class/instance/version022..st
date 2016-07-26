versions
version022: spec
	<version: '0.2.2' imports: #('0.1.0-baseline')>

	spec for: #common do: [
		spec blessing: #release.
		spec
			package: 'ShoreLine-Report-Core' with: 'ShoreLine-Report-Core-TommasoDalSasso.11';
			package: 'ShoreLine-Report-UI' with: 'ShoreLine-Report-UI-TommasoDalSasso.16';
			package: 'ShoreLine-Report-Settings' with: 'ShoreLine-Report-Settings-TommasoDalSasso.2' ]