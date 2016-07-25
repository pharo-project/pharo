lessons
createConfigurationAndBaseline
	^Lesson
		title: '1. Create configuration'
		lesson: '"The MetacelloToolBox class provides programmatic support for creating and maintaining your configuration. 

Use the following expression to create your configuration class and initial baseline version. Edit the expression to specifiy the required projects, packages, dependencies and groups for your project:"
	
	MetacelloToolBox
		createBaseline: ''1.0-baseline''
		for: ''Example''
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
			(''Core Tests'' -> #(''Core'' ''Tests''))}.
			
"After evaluating the above expression, browse the configuration:"

	Smalltalk tools browser fullOnClass: ConfigurationOfExample selector: #baseline10:.
	
"Edit the #baseline10 method in the browser to fine tune the baseline specification."

ProfStef next.
'