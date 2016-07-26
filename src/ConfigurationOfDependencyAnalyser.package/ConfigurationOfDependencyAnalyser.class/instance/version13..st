versions
version13: spec
	<version: '1.3' imports: #('0.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.3'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '6/18/2015 16:28'.
		spec 
			package: #'Tool-DependencyAnalyser' with: 'Tool-DependencyAnalyser-ChristopheDemarey.6';
			package: #'Tool-DependencyAnalyser-Test' with: 'Tool-DependencyAnalyser-Test-ChristopheDemarey.3';
			package: #'Tool-DependencyAnalyser-UI' with: 'Tool-DependencyAnalyser-UI-ChristopheDemarey.6'. ].
