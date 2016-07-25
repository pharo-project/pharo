versions
version19: spec
	<version: '1.9' imports: #('0.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.9'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '9/1/2015 13:27'.
		spec 
			package: #'Tool-DependencyAnalyser' with: 'Tool-DependencyAnalyser-ChristopheDemarey.15';
			package: #'Tool-DependencyAnalyser-Test' with: 'Tool-DependencyAnalyser-Test-ChristopheDemarey.6';
			package: #'Tool-DependencyAnalyser-UI' with: 'Tool-DependencyAnalyser-UI-ChristopheDemarey.27';
			package: #'Tool-DependencyAnalyser-Checker' with: 'Tool-DependencyAnalyser-Checker-ChristopheDemarey.1'. ].
