versions
version114: spec
	<version: '1.14' imports: #('0.6-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.14'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '10/01/2015 16:40'.
		spec 
			project: 'MooseAlgos' with: '2.5-baseline';
			project: 'NeoJSON' with: '11'.
		spec 
			package: #'Tool-DependencyAnalyser' with: 'Tool-DependencyAnalyser-ChristopheDemarey.25';
			package: #'Tool-DependencyAnalyser-Test' with: 'Tool-DependencyAnalyser-Test-ChristopheDemarey.11';
			package: #'Tool-DependencyAnalyser-UI' with: 'Tool-DependencyAnalyser-UI-ChristopheDemarey.36';
			package: #'Tool-DependencyAnalyser-HTMLReport' with: 'Tool-DependencyAnalyser-HTMLReport-ChristopheDemarey.1'. ].