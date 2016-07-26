baselines
baseline15: spec
	<version: '1.5-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec description: 'Implements commit configuration and commit all actions.
Fix the browse version command.
Update ''Save Configuration'' command.
Fix pb in Save config command.
Add a convertor constructor method on MTProject.
Rename the parameter constructor.
Update commands that can be used on a configuration.
Clean up in Version commands.
Commands cleanup #2'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '10/25/2013 19:05'.
		spec repository: 'http://smalltalkhub.com/mc/demarey/Versionner/main'.
		spec 
			package: 'Versionner-Core-Model';
			package: 'Versionner-Core-Commands' with: [
				spec requires: #('Versionner-Core-Model' ). ];
			package: 'Versionner-Core-DependenciesModel' with: [
				spec requires: #('Versionner-Core-Model' 'Versionner-Core-Announcements' ). ];
			package: 'Versionner-Core-Announcements' with: [
				spec requires: #('Versionner-Core-Model' ). ];
			package: 'Versionner-Spec-Browser';
			package: 'Versionner-Tests-Resources';
			package: 'Versionner-Tests-Core-Model' with: [
				spec requires: #('Versionner-Core-Model' ). ];
			package: 'Versionner-Tests-Core-Commands' with: [
				spec requires: #('Versionner-Core-Model' 'Versionner-Core-Commands' ). ];
			package: 'Versionner-Tests-Core-DependenciesModel' with: [
				spec requires: #('Versionner-Core-DependenciesModel' ). ];
			package: 'Versionner-Help'.
		spec group: 'Core' with: #('Versionner-Core-Model' 'Versionner-Core-DependenciesModel' 'Versionner-Core-Commands' 'Versionner-Core-Announcements' ). ].
