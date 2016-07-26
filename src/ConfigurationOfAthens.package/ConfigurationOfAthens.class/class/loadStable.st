loading
loadStable
	"Load the #stable version defined for this platform. The #development version will change over time and is not expected to be stable."

	"self loadStable"

	<apiDocumentation>
	^(self project latestVersion: #stable) load