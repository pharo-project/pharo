lessons
create10Baseline

	^Lesson
		title: 'Create 1.0 Baseline'
		lesson: 
'"
	1. Create configuration
		- class named ConfigurationOfExample created if it does not already exist.
	2. Prepare to define baseline version method
		- selector, category and version number defined
	3. Define baseline project structure
		- repository
		- external project ''Shout''
		- packages ''ProfStef-Core'' and ''ProfStef-Tests'', plus dependencies
	4. Compile the #baseline10: method
		- version method compiled and validated "
	
"1."	(MetacelloToolBox configurationNamed: ''Example'')
"2."		createVersionMethod: ''baseline10:'' inCategory: ''baselines'' forVersion: ''1.0-baseline'';
"3."		addSection: #common
			repository: ''http://www.squeaksource.com/ProfStef''
			requiredProjects: #(''Shout'')
			packages: #(''ProfStef-Core'' ''ProfStef-Tests'')
			dependencies:
				{(''ProfStef-Core'' -> #(''Shout'')).
				(''ProfStef-Tests'' -> #(''ProfStef-Core''))}
			groups:
				{(''default'' -> #(''Core'')).
				(''Core'' -> #(''ProfStef-Core'')).
				(''Tests'' -> #(''ProfStef-Tests'')).
				(''Core Tests'' -> #(''Core'' ''Tests''))}
			versionSpecsDo: [ :versionSpec | versionSpec blessing: #baseline ];
"4."		commitMethod.

"After evaluating the above expression, browse the configuration:"

	ConfigurationOfExample browse.
	
"and look at the generated method #baseline10:. Note that the symbolic version #bleedingEdge is used for the Shout project."

ProfStef next.
'
