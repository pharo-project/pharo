Class {
	#name : 'BaselineOfSystemVersion',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfSystemVersion',
	#package : 'BaselineOfSystemVersion'
}

{ #category : 'baselines' }
BaselineOfSystemVersion >> baseline: spec [

	<baseline>
	| repository |
   repository := self packageRepositoryURLForSpec: spec.
	spec for: #common do: [
			spec baseline: 'ClapTests' with: [
            		spec
               		repository: repository;
						className: 'BaselineOfClap';
               		loads: #( 'Tests' ) ].
			spec
				baseline: 'SystemSupport' with: [ spec repository: repository ];
				package: 'Regex-Core';
				package: 'System-Version' with: [ spec requires: #( 'SystemSupport' ) ];
				package: 'System-Version-Tests' with: [ spec requires: #( 'ClapTests' 'System-Version' 'Regex-Core' ) ].

			spec
				group: 'Core' with: { 'System-Version' };
				group: 'Tests' with: { 'System-Version-Tests' } ]
			
]
