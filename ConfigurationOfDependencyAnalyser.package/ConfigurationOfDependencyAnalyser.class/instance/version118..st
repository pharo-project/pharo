versions
version118: spec
	<version: '1.18' imports: #('0.6-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.18
			Fix printString of DADependencyWrapper.
			Fix a bug in DAPackageUnderAnalysisNode>>#children: ensure that we work on a copy of the collection.
			Sort root nodes.'.
		spec author: 'ChristopheDemarey'.
		spec timestamp: '4/22/2016 14:09'.
		spec 
			project: 'MooseAlgos' with: '2.5-baseline';
			project: 'NeoJSON' with: '12'.
		spec 
			package: #'Tool-DependencyAnalyser' with: 'Tool-DependencyAnalyser-ChristopheDemarey.28';
			package: #'Tool-DependencyAnalyser-Test' with: 'Tool-DependencyAnalyser-Test-ChristopheDemarey.11';
			package: #'Tool-DependencyAnalyser-UI' with: 'Tool-DependencyAnalyser-UI-ChristopheDemarey.43';
			package: #'Tool-DependencyAnalyser-HTMLReport' with: #'Tool-DependencyAnalyser-HTMLReport'. ].
