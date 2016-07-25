ConfigurationOfMBFooTest
version12MBFooTest: spec
	"self reset"
	<version: '1.2' imports: #('1.0-baseline')>

	spec for: #'common' do: [
		spec blessing: #release.
		spec package: 'MBFooTests' with: 'MBFooTests-dkh.3'
].
