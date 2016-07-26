baselines
baseline02: spec
	<version: '0.2-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'http://smalltalkhub.com/mc/PharoExtras/Tool-DependencyAnalyser/main/'.
		spec 
			package: #'Tool-DependencyAnalyser';
			package: #'Tool-DependencyAnalyser-Test' with: [
				spec requires: #(#'Tool-DependencyAnalyser' ). ];
			package: #'Tool-DependencyAnalyser-UI' with: [
				spec requires: #(#'Tool-DependencyAnalyser' ). ]. ].
