versions
version117: spec
	<version: '1.17' imports: #('0.6-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 1.17'.
		spec author: 'PavelKrivanek'.
		spec timestamp: '3/30/2016 16:00'.
		spec 
			project: 'MooseAlgos' with: '2.5-baseline';
			project: 'NeoJSON' with: '12'.
		spec 
			package: #'Tool-DependencyAnalyser' with: 'Tool-DependencyAnalyser-PavelKrivanek.27';
			package: #'Tool-DependencyAnalyser-Test' with: 'Tool-DependencyAnalyser-Test-ChristopheDemarey.11';
			package: #'Tool-DependencyAnalyser-UI' with: 'Tool-DependencyAnalyser-UI-ChristopheDemarey.40';
			package: #'Tool-DependencyAnalyser-HTMLReport' with: #'Tool-DependencyAnalyser-HTMLReport-ChristopheDemarey.1'. ].
