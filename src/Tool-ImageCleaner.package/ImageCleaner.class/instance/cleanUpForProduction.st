api
cleanUpForProduction
	"self new cleanUpForProduction"
	"delete logo"	
	PolymorphSystemSettings showDesktopLogo: false.

	self packagesForCleanUpInProduction do: [ :each | (MCPackage named: each) unload ].	
	self helpPackages do: [ :each | (MCPackage named: each) unload ].	
	self testPackages do: [ :each | (MCPackage named: each) unload ].
	self examplePackages do: [ :each | (MCPackage named: each) unload ].
	(MCPackage named: 'ScriptLoader40') unload.	
		
		
	(MCPackage named: 'MonticelloMocks') unload.	
	(MCPackage named: 'ToolsTest') unload.	
	(MCPackage named: 'Announcements-Tests-Core') unload.	
	(MCPackage named: 'AST-Tests-Core') unload.	
	(MCPackage named: 'AST-Interpreter-Test') unload.	
	(MCPackage named: 'Ring-Tests-Containers') unload.	
	(MCPackage named: 'Ring-Tests-Kernel') unload.	
	(MCPackage named: 'Ring-Tests-Monticello') unload.	
	(MCPackage named: 'Regex-Tests-Core') unload.	
	(MCPackage named: 'Refactoring-Tests-Changes') unload.	
	(MCPackage named: 'Refactoring-Tests-Core') unload.	
	(MCPackage named: 'Refactoring-Tests-Critics') unload.	
	(MCPackage named: 'Refactoring-Tests-Environment') unload.	
	(MCPackage named: 'FileSystem-Tests-Core') unload.	
	(MCPackage named: 'FileSystem-Tests-Disk') unload.	
	(MCPackage named: 'FileSystem-Tests-Memory') unload.		
			
	(MCPackage named: 'GT-Tests-Inspector') unload.		
	(MCPackage named: 'Glamour-Tests-Core') unload.		
	(MCPackage named: 'Glamour-Tests-Morphic') unload.		
	(MCPackage named: 'Glamour-Tests-Resources') unload.		
	(MCPackage named: 'Glamour-Tests-Rubric') unload.		
			
	(MCPackage named: 'Versionner-Core-Model') unload.
	(MCPackage named: 'Versionner-Core-DependenciesModel') unload.		
	(MCPackage named: 'Versionner-Core-Announcements') unload.	
	(MCPackage named: 'Versionner-Core-Commands') unload.	
	(MCPackage named: 'Versionner-Spec-Browser') unload.				
	(MCPackage named: 'Versionner-Tests-Core-Commands') unload.	
	(MCPackage named: 'Versionner-Tests-Core-DependenciesModel') unload.	
	(MCPackage named: 'Versionner-Tests-Core-Model') unload.			
	(MCPackage named: 'Versionner-Tests-Resources') unload.												
	
	self cleanUpForRelease.
	