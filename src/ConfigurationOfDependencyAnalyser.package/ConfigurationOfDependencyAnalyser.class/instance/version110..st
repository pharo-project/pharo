versions
version110: spec
	<version: '1.10' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.10'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '9/4/2015 17:25'.
		spec 
			package: #'Tool-DependencyAnalyser' with: 'Tool-DependencyAnalyser-ChristopheDemarey.18';
			package: #'Tool-DependencyAnalyser-Test' with: 'Tool-DependencyAnalyser-Test-ChristopheDemarey.6';
			package: #'Tool-DependencyAnalyser-UI' with: 'Tool-DependencyAnalyser-UI-ChristopheDemarey.29';
			package: #'Tool-DependencyAnalyser-Checker' with: 'Tool-DependencyAnalyser-Checker-ChristopheDemarey.19'. ].
