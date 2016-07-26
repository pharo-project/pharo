versions
version16: spec
	<version: '1.6' imports: #('0.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.6'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '8/7/2015 12:24'.
		spec 
			package: #'Tool-DependencyAnalyser' with: 'Tool-DependencyAnalyser-ChristopheDemarey.11';
			package: #'Tool-DependencyAnalyser-Test' with: 'Tool-DependencyAnalyser-Test-ChristopheDemarey.5';
			package: #'Tool-DependencyAnalyser-UI' with: 'Tool-DependencyAnalyser-UI-ChristopheDemarey.21'. ].
