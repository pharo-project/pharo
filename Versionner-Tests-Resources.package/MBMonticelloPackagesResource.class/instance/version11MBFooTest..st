ConfigurationOfMBFooTest
version11MBFooTest: spec
	"self reset"
	<version: '1.1' imports: #('1.0-baseline')>

	spec for: #'common' do: [
		spec blessing: #release.
		spec package: 'MBFooTests' with: 'MBFooTests-dkh.2'
].
