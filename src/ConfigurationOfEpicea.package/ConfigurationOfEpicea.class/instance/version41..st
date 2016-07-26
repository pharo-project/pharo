versions
version41: spec
	<version: '4.1' imports: #('4.1-baseline') >

	spec for: #'common' do: [
		spec description: '
	* The base for files is a FileLocator instead of a FileReference. This way it should tolerate sending an image from one computer to another with epicea already enabled inside.
	* Support for multiple themes, e.g. the DarkTheme.
'.
		spec author: 'MartinDias'.
		spec timestamp: '17/07/2014'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.162';
			package: 'Epicea' with: 'Epicea-MartinDias.620'. ].