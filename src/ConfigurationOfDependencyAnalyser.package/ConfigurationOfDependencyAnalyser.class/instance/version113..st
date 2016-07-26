versions
version113: spec
	<version: '1.13' imports: #('0.5-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.13'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '9/16/2015 16:10'.
		spec 
			project: 'MooseAlgos' with: '2.5-baseline';
			project: 'NeoJSON' with: '11'.
		spec 
			package: #'Tool-DependencyAnalyser' with: 'Tool-DependencyAnalyser-ChristopheDemarey.22';
			package: #'Tool-DependencyAnalyser-Test' with: 'Tool-DependencyAnalyser-Test-ChristopheDemarey.10';
			package: #'Tool-DependencyAnalyser-UI' with: 'Tool-DependencyAnalyser-UI-ChristopheDemarey.36';
			package: #'Tool-DependencyAnalyser-Checker' with: #'Tool-DependencyAnalyser-Checker-ChristopheDemarey.38'. ].
