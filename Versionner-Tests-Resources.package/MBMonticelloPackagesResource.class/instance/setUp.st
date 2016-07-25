running
setUp
	"self reset"

	super setUp.
	self
		setUpMonticelloRepository;
		setUpPackagesMBFooTests;
		setUpConfigurationOfMBFooTest;
		setUpPackagesMBCommandFooTests;
		setUpConfigurationOfMBFooIssue117;
		setUpConfigurationOfMBBranchTest