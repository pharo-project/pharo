baseline 1.0
baseline10: spec 
	<version: '1.0-baseline'>
	
	spec for: #common do: [
		"alternate methods for specifying author, blessing, description, timestamp, preLoadDoIt, postLoadDoit (not recommended)"
		spec blessing: [ spec value: #baseline. ].
		spec description: [ spec value: 'Descriptive comment'. ].
		spec author: [ spec value: 'dkh'. ].
		spec timestamp: [ spec value: '10/7/2009 14:40'. ].
		spec timestamp: [ spec value: (DateAndTime fromString: '10/7/2009 14:40'). ].
		"recommended methods for specifying author, blessing, description, timestamp, preLoadDoIt, postLoadDoit"
		"#development, #baseline, #release, #beta, etc."
		spec blessing: #baseline.									
		spec description: 'Descriptive comment'.
		spec author: 'dkh'.
		spec timestamp: (DateAndTime fromString: '10/7/2009 14:40').
		spec timestamp: '10/7/2009 14:40'.
		spec
			"Before loading packages or projects in this version, send #preloadForVersion to an instance of this config"
			preLoadDoIt: #preloadForVersion;
			"After loading packages or projects in this version, send #postloadForVersion to an instance of this config"
			postLoadDoIt: #postloadForVersion.
		spec repository: 'http://www.example.com/rr'.
		spec repository: 'http://www.example.com/private' username: 'foo' password: 'bar'.
		spec repositories: [
			"One or more repositories may be defined (MCReposioryGroup). Previous forms useful when specifying a single reposiory."
			spec 
				"directory-based repositories"
				repository: '/opt/mc/repository';
				repository: 'c:\pharo\myapp\repo\';
				"ftp-based repository ... note, supported in Pharo/Squeak only"					
				repository: 'ftp://ftp.example.com/repo';
				"dictionary-based repository ... repository stored at `Smalltalk at: #GlobalName`"		
				repository: 'dictionary://GlobalName';				
				repository: 'http://www.example.com/rr';
				repository: 'http://www.example.com/ar';
				repository: 'http://www.example.com/private' username: 'foo' password: 'bar' ].
		spec
			"Create or update a project reference named 'UI Support'"
			project: 'UI Support' with: [							
				"One or more of the following attributes may be defined or changed"
				spec
					"OPTIONAL: Name of config class (i.e., ConfigurationOfXXX), if not specified, className is assumed to be the 
					 name of the project prependended with 'ConfigurationOf'"
					className: 'ConfigurationOfUI';
					"Version of project to be loaded. if theversionString is not specified, then the latest version of the project is used."
					versionString: '1.0';
					"Before loading this project, send #preloadForProject to an instance of this config"
					preLoadDoIt: #preloadForProject;
					"After loading this project, send #postloadForProject to an instance of this config"
					postLoadDoIt: #postloadForProject;
					"OPTIONAL: Version comparison operator #= #~= #> #< #>= #<= #~> "
					operator: #~>; 	
					"OPTIONAL: List of packages to be loaded from project"															
					loads: #('UI-Core' );	
					"Optional: Name of package containing the config, by convention same as className"						
					file: 'ConfigurationOfUI';	
					"Repository where package resides"					
					repository: 'http://www.example.com/r' ];	
			"Create a new project reference to replace existing project reference"
			project: 'UI Support' overrides: [					
				"One or more of the following attributes may be defined"
				spec
					className: 'ConfigurationOfUINew';
					versionString: '1.0';
					operator: #>=;
					loads: #('UI-Core' 'UI-Tests' );
					repository: 'http://www.example.com/r' username: 'foo' password: 'bar' ];
			"Create project reference named 'UI Tests'"
		 	project: 'UI Tests' 
				"based on c copy of 'UI Support' project"									
				copyFrom: 'UI Support' 								
				with: [
					"One or more of the following attributes may be changed"
					spec
						className: 'ConfigurationOfUI';
						versionString: '1.0';
						operator: #~>;
						loads: #('UI-Core' 'UI-Tests' );
						repository: 'http://www.example.com/r'];
			"Change the versionString for 'UI Support' to '1.0.1'"
			project: 'UI Support' with: '1.0.1';	
			"Remove the project reference 'UI Tests'"				
			removeProject: 'UI Tests';
			"Multiple repositories for configuration - config may be found in either repository. 
			 Secondary repository is useful when primary repository may not be available"
			project: 'UI Multi' with: [							
				spec
					className: 'ConfigurationOfMulti';
					repository: 'http://www.example.com/r';
					repository: 'http://www.example.com/s' ].
		"Create or update 'Example-AddOn' package"								
		spec 
			package: 'Example-AddOn' with: [						
				"One or more of the following attributes may be defined or changed"
				spec
					"'Example-Core' must be loaded before 'Example-AddOn'" 
					requires: #('Example-Core' );
					"When 'Example-AddOn' is loaded, load 'Example-UI'"				
					includes: #('Example-UI' );
					"Explicitly oad version 'Example-AddOn-anon.3' of the package"					
					file: 'Example-AddOn-anon.3';					
					repositories: [
						spec 
							"Load 'Example-AddOn' from  the 'http://www.example.com/yar'"
							repository: 'http://www.example.com/yar'; 
							"or 'http://www.example.com/yas' repositores" 
							repository: 'http://www.example.com/yas']; 
					"Before loading 'Example-AddOn' send #preloadForAddOn to an instance of this config"
					preLoadDoIt: #preloadForAddOn;
					"After loading 'Example-AddOn' send #postloadForAddOn to an instance of this config"			
					postLoadDoIt: #postloadForAddOn ];
			"Create a new package to replace existing package"			
			package: 'Example-AddOn' overrides: [					
				"One or more of the following attributes may be defined"
				spec 
					requires: #('Example-Core' 'UI Support' );
					includes: #('Example-UI' );
					file: 'Example-AddOn-anon.7';
					supplyingAnswers: #( #('list of packages' 'Kernel* Collection*'));
					repository: 'http://www.example.com/or' username: 'foo' password: 'bar' ;
					preLoadDoIt: #preloadForAddOn;
					postLoadDoIt: #postloadForAddOn ];
			"Change the package version loaded"
			package: 'Example-AddOn' with:'Example-AddOn-anon.5';	
			"Create 'Example-Core' package in project"					
			package: 'Example-Core';								
			package: 'Example-Tests' with: [
				spec requires: #('Example-Core' ) ];
			package: 'Example-TestsUI' with: [
				spec requires: #('Example-UI' ) ];
			package: 'Example-UI' with: [
				spec requires: #('Example-AddOn' ) ];
			"Remove the package 'Example-Tests'"
			removePackage: 'Example-Tests'.						
		spec
			"Create or update the group 'default', adding 'Example-Core' and 'Example-AddOn' to the group."
			group: 'default' 
				with: 	#('Example-Core' 'Example-AddOn' );
			"Create the group 'default' to replace existing group, consisting of 'Example-Core' and 'Example-AddOn'"	
			group: 'default' 
				overrides:  #('Example-Core' 'Example-Tests' );	
			"Remove the group 'default'"
			removeGroup: 'default' ].									
