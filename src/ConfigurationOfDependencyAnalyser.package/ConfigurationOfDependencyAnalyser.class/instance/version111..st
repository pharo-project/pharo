versions
version111: spec
	<version: '1.11' imports: #('0.4-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.11'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '9/9/2015 11:21'.
		spec project: 'MooseAlgos' with: '2.6'.
		spec 
			package: #'Tool-DependencyAnalyser' with: 'Tool-DependencyAnalyser-ChristopheDemarey.20';
			package: #'Tool-DependencyAnalyser-Test' with: 'Tool-DependencyAnalyser-Test-ChristopheDemarey.10';
			package: #'Tool-DependencyAnalyser-UI' with: 'Tool-DependencyAnalyser-UI-ChristopheDemarey.33';
			package: #'Tool-DependencyAnalyser-Checker' with: 'Tool-DependencyAnalyser-Checker-ChristopheDemarey.30'. ].
