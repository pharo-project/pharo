versions
version15: spec
	<version: '1.5' imports: #('0.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.5'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '7/2/2015 15:48'.
		spec 
			package: #'Tool-DependencyAnalyser' with: 'Tool-DependencyAnalyser-ChristopheDemarey.10';
			package: #'Tool-DependencyAnalyser-Test' with: 'Tool-DependencyAnalyser-Test-ChristopheDemarey.5';
			package: #'Tool-DependencyAnalyser-UI' with: 'Tool-DependencyAnalyser-UI-ChristopheDemarey.13'. ].
