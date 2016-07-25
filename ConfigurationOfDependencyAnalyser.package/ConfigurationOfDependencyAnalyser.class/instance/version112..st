versions
version112: spec
	<version: '1.12' imports: #('0.4-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.12'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '9/11/2015 13:55'.
		spec project: 'MooseAlgos' with: '2.5-baseline'.
		spec 
			package: #'Tool-DependencyAnalyser' with: 'Tool-DependencyAnalyser-ChristopheDemarey.20';
			package: #'Tool-DependencyAnalyser-Test' with: 'Tool-DependencyAnalyser-Test-ChristopheDemarey.10';
			package: #'Tool-DependencyAnalyser-UI' with: 'Tool-DependencyAnalyser-UI-ChristopheDemarey.35';
			package: #'Tool-DependencyAnalyser-Checker' with: 'Tool-DependencyAnalyser-Checker-ChristopheDemarey.33'. ].
