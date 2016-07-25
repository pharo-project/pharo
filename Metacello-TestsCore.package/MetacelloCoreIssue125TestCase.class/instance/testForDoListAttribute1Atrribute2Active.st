tests
testForDoListAttribute1Atrribute2Active
  "https://github.com/dalehenrich/metacello-work/issues/125"

  | project version expected actual |
  project := self projectWith: #(#'attribute1' #'attribute2').
  version := project version: '3.0-baseline'.
  expected := 'spec blessing: #''baseline''.
spec preLoadDoIt: #''preloadForCore''.
spec postLoadDoIt: #''postloadForCore:package:''.
spec 
	project: ''Example Project'' with: [
		spec
			className: ''ConfigurationOfExampleProject'';
			versionString: ''1.0-baseline'';
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'';
			loads: #(''core'' ) ];
	project: ''Extra Project'' with: [
		spec
			className: ''ConfigurationOfExtraProject'';
			versionString: ''1.0-baseline'';
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'';
			loads: #(''core'' ) ];
	project: ''Extra Project'' with: [
		spec
			className: ''ConfigurationOfExtraProject'';
			versionString: ''1.0-baseline'';
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'';
			loads: #(''core'' ) ].
spec 
	group: ''Core'' with: #(''Example Project'' );
	group: ''Core'' with: #(''Extra Project'' );
	group: ''Core'' with: #(''Extra Project'' ).'.
  actual := version spec printString.
  self assert: expected = actual