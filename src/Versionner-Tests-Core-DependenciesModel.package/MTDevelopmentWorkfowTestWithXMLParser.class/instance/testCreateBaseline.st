tests
testCreateBaseline
	| baselineName baseline |
	
	baselineName := '84.1'.
	project devWorkflow createBaseline: baselineName. 
	
	"Visit the created baseline"
	baseline := project configurationClass project version: baselineName.
	project := visitor visitConfiguration: project configurationClass withVersion: baseline.
	self assert: project notNil.
	self assert: project name equals: 'VersionnerTestXMLParser'.
	self assert: project version name equals: baselineName.
