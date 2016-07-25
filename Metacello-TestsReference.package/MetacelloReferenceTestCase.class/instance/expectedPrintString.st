testing
expectedPrintString
    ^ 'spec blessing: #''baseline''.
spec description: ''Descriptive comment''.
spec preLoadDoIt: #''preloadForVersion''.
spec postLoadDoIt: #''postloadForVersion''.
spec author: ''dkh''.
spec timestamp: ''10/7/2009 14:40''.
spec repositories: [
	spec
		repository: ''/opt/mc/repository'';
		repository: ''c:\pharo\myapp\repo\'';
		repository: ''dictionary://GlobalName'';
		repository: ''ftp://ftp.example.com/repo'';
		repository: ''http://www.example.com/ar'';
		repository: ''http://www.example.com/private'' username: ''foo'' password: ''bar'';
		repository: ''http://www.example.com/rr'' ].
spec 
	project: ''UI Support'' with: [
		spec
			className: ''ConfigurationOfUI'';
			versionString: ''1.0'';
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'';
			operator: #''~>'';
			loads: #(''UI-Core'' );
			repository: ''http://www.example.com/r'' ];
	project: ''UI Support'' overrides: [
		spec
			className: ''ConfigurationOfUINew'';
			versionString: ''1.0'';
			operator: #''>='';
			loads: #(''UI-Core'' ''UI-Tests'' );
			repository: ''http://www.example.com/r'' username: ''foo'' password: ''bar'' ];
	project: ''UI Tests'' copyFrom: ''UI Support'' with: [
		spec
			className: ''ConfigurationOfUI'';
			versionString: ''1.0'';
			operator: #''~>'';
			loads: #(''UI-Core'' ''UI-Tests'' );
			repository: ''http://www.example.com/r'' ];
	project: ''UI Support'' with: ''1.0.1'';
	removeProject: ''UI Tests'';
	project: ''UI Multi'' with: [
		spec
			className: ''ConfigurationOfMulti'';
			repository: ''http://www.example.com/r'';
			repository: ''http://www.example.com/s'' ].
spec 
	package: ''Example-AddOn'' with: [
		spec 
			requires: #(''Example-Core'' );
			includes: #(''Example-UI'' );
			file: ''Example-AddOn-anon.3'';
			repositories: [
				spec
					repository: ''http://www.example.com/yar'';
					repository: ''http://www.example.com/yas'' ];
			preLoadDoIt: #''preloadForAddOn'';
			postLoadDoIt: #''postloadForAddOn''. ];
	package: ''Example-AddOn'' overrides: [
		spec 
			requires: #(''Example-Core'' ''UI Support'' );
			includes: #(''Example-UI'' );
			supplyingAnswers: #( #( ''list of packages'' ''Kernel* Collection*'' ) );
			file: ''Example-AddOn-anon.7'';
			repository: ''http://www.example.com/or'' username: ''foo'' password: ''bar'';
			preLoadDoIt: #''preloadForAddOn'';
			postLoadDoIt: #''postloadForAddOn''. ];
	package: ''Example-AddOn'' with: ''Example-AddOn-anon.5'';
	package: ''Example-Core'';
	package: ''Example-Tests'' with: [
		spec requires: #(''Example-Core'' ). ];
	package: ''Example-TestsUI'' with: [
		spec requires: #(''Example-UI'' ). ];
	package: ''Example-UI'' with: [
		spec requires: #(''Example-AddOn'' ). ];
	removePackage: ''Example-Tests''.
spec 
	group: ''default'' with: #(''Example-Core'' ''Example-AddOn'' );
	group: ''default'' overrides: #(''Example-Core'' ''Example-Tests'' );
	removeGroup: ''default''.'