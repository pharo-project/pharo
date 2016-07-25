tests
testBasicBleedingEdgeVersion
    | project version wc expected actual |
    project := self projectWith: #(#'platformVersion1.x').
    version := project version: #'bleedingEdge'.
    expected := 'spec blessing: #''baseline''.
spec project: ''Example Project'' with: [
		spec
			className: ''MetacelloExampleProjectConfig'';
			versionString: ''1.0-baseline'';
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'' ].'.
    actual := version spec printString.
    self assert: expected = actual