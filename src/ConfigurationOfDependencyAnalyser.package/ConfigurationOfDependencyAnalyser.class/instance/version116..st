versions
version116: spec
	<version: '1.16' imports: #('0.6-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.16'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '3/25/2016 11:51'.
		spec 
			project: 'MooseAlgos' with: '2.5-baseline';
			project: 'NeoJSON' with: '12'.
		spec 
			package: #'Tool-DependencyAnalyser' with: 'Tool-DependencyAnalyser-ChristopheDemarey.26';
			package: #'Tool-DependencyAnalyser-Test' with: 'Tool-DependencyAnalyser-Test-ChristopheDemarey.11';
			package: #'Tool-DependencyAnalyser-UI' with: 'Tool-DependencyAnalyser-UI-ChristopheDemarey.40';
			package: #'Tool-DependencyAnalyser-HTMLReport' with: #'Tool-DependencyAnalyser-HTMLReport-ChristopheDemarey.1'. ].
