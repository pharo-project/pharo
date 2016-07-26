versions
version115: spec
	<version: '1.15' imports: #('0.6-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.15'.
		spec author: 'GuillermoPolito'.
		spec timestamp: '2/9/2016 15:18'.
		spec 
			project: 'MooseAlgos' with: '2.5-baseline';
			project: 'NeoJSON' with: '12'.
		spec 
			package: #'Tool-DependencyAnalyser' with: 'Tool-DependencyAnalyser-ChristopheDemarey.25';
			package: #'Tool-DependencyAnalyser-Test' with: 'Tool-DependencyAnalyser-Test-ChristopheDemarey.11';
			package: #'Tool-DependencyAnalyser-UI' with: 'Tool-DependencyAnalyser-UI-GuillermoPolito.39';
			package: #'Tool-DependencyAnalyser-HTMLReport'with: 'Tool-DependencyAnalyser-HTMLReport-ChristopheDemarey.1'. ].
