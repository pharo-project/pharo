Extension { #name : #DrTestsPlugin }

{ #category : #'*DrTests-TestsRunner' }
DrTestsPlugin >> buildReRunConfigurationFrom: aDrTests [
	"Builds a configuration for the plugin by reading the information held by the UI (aDrTests) and the last value of results."

	^ DTReRunConfiguration new
		originalConfiguration: aDrTests drTests testsConfiguration;
		previousResult: aDrTests drTests pluginResult;
		configurationToRun: (DTTestsRunnerConfiguration items: aDrTests resultSelected contentForReRun);
		yourself
]
