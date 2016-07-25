tests
testBasicStableVersion
    | project version wc expected actual |
    project := self projectWith: #(#'platformVersion1.x').
    version := project version: #'stable'.
    expected := 'spec blessing: #''release''.
spec project: ''Example Project'' with: [
		spec
			className: ''MetacelloExampleProjectConfig'';
			versionString: ''1.0-baseline'';
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'' ].'.
    actual := version spec printString.
    self assert: expected = actual