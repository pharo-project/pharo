ConfigurationOfMBFooTest
version10MBFooTest: spec
	"self reset"
	<version: '1.0' imports: #('1.0-baseline')>

	spec for: #'common' do: [
		spec blessing: #release.
		spec package: 'MBFooTests' with: 'MBFooTests-dkh.1'
].
