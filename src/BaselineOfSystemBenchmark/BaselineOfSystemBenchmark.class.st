Class {
	#name : 'BaselineOfSystemBenchmark',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfSystemBenchmark',
	#package : 'BaselineOfSystemBenchmark'
}

{ #category : 'baselines' }
BaselineOfSystemBenchmark >> baseline: spec [

	<baseline>
	spec for: #common do: [
		spec baseline: 'SUnit' with: [
			spec
				repository: (self packageRepositoryURLForSpec: spec);
				loads: 'SUnit-Core' ].
		spec
			package: 'System-Benchmark';
			package: 'System-Benchmark-Tests'
			with: [ spec requires: #( 'SUnit' 'System-Benchmark' ) ].

		spec
			group: 'default' with: #( 'System-Benchmark' );
			group: 'tests' with: #( 'System-Benchmark-Tests' ) ]
]
