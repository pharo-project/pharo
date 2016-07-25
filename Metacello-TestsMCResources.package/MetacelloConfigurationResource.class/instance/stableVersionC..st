ConfigurationOfSymbolic
stableVersionC: spec 
	"self reset"
	<symbolicVersion: #stable>
	
	spec for: #'common' do: [ spec version: '4.2'].
	spec for: #'platformVersion1.x' do: [ spec version: '4.0'].
	spec for: #'platformVersion1.0.x' do: [ spec version: '4.1'].
	spec for: #'platformVersion1.1.x' do: [ spec version: '4.2'].
	spec for: #'platformVersion1.2.x' do: [ spec version: '4.3'].
