testing
testReferenceConfig

	| spec map string expected |
	spec := (MetacelloReferenceConfig project version: '1.0-baseline') spec.
	map := spec packages map.
	string := self conditionResult: (map at: 'Example-AddOn') printString.
   	expected := 'spec 
	name: ''Example-AddOn'';
	requires: #(''Example-Core'' ''UI Support'' );
	includes: #(''Example-UI'' );
	supplyingAnswers: #( #( ''list of packages'' ''Kernel* Collection*'' ) );
	file: ''Example-AddOn-anon.5'';
	repository: ''http://www.example.com/or'' username: ''foo'' password: ''bar'';
	preLoadDoIt: #''preloadForAddOn'';
	postLoadDoIt: #''postloadForAddOn''.'.
	self assert: string = expected.

	string := (map at: 'Example-Core') printString.
  	self assert: string = 'spec name: ''Example-Core'''.
	
	string := (map at: 'Example-TestsUI') printString.
  	self assert: string =  'spec 
	name: ''Example-TestsUI'';
	requires: #(''Example-UI'' ).'.

	string := (map at: 'UI Support') printString.
  	self assert: string = 'spec 
	name: ''UI Support'';
	projectReference: [
		spec
			className: ''ConfigurationOfUINew'';
			versionString: ''1.0.1'';
			operator: #''>='';
			loads: #(''UI-Core'' ''UI-Tests'' );
			repository: ''http://www.example.com/r'' username: ''foo'' password: ''bar'' ].'.
