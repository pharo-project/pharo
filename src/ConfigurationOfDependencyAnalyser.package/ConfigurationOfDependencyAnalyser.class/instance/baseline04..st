baselines
baseline04: spec
	<version: '0.4-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'http://smalltalkhub.com/mc/PharoExtras/Tool-DependencyAnalyser/main/'.
		spec project: 'MooseAlgos' with: [
				spec
					className: #ConfigurationOfMooseAlgos;
					versionString: #'bleedingEdge';
					loads: #('Moose-Algos-Graph' );
					repository: 'http://smalltalkhub.com/mc/Moose/MooseAlgos/main/' ].
		spec 
			package: #'Tool-DependencyAnalyser';
			package: #'Tool-DependencyAnalyser-Test' with: [
				spec requires: #(#'Tool-DependencyAnalyser' ). ];
			package: #'Tool-DependencyAnalyser-UI' with: [
				spec requires: #(#'Tool-DependencyAnalyser' ). ];
			package: #'Tool-DependencyAnalyser-Checker' with: [
				spec requires: #(#'Tool-DependencyAnalyser' 'MooseAlgos' ). ].
		spec group: 'checker' with: #(#'Tool-DependencyAnalyser-Checker' ). ].
