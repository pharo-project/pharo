versions
version11: spec
	<version: '1.1' imports: #('0.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.1'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '3/16/2015 14:10'.
		spec 
			package: #'Tool-DependencyAnalyser' with: 'Tool-DependencyAnalyser-StephaneDucasse.1';
			package: #'Tool-DependencyAnalyser-Test' with: 'Tool-DependencyAnalyser-Test-StephaneDucasse.1';
			package: #'Tool-DependencyAnalyser-UI' with: #'Tool-DependencyAnalyser-UI-ChristopheDemarey.1'. ].
