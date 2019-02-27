Class {
	#name : #BaselineOfSpec,
	#superclass : #BaselineOf,
	#instVars : [
		'link'
	],
	#category : #BaselineOfSpec
}

{ #category : #'pre/post load' }
BaselineOfSpec >> addHackToLoadSpec [
	"Metacello pop some UIManager request during the loading of Spec. This cause a problem so we remove the need of the UI manager."

	self shouldInstallHack ifFalse: [ ^ self ].
	
	link := MetaLink new
		metaObject: #receiver;
		selector: #warnAboutLosingChangesForSpec:;
		arguments: #(#arguments);
		control: #instead.
		
	(MCVersionLoader >> #warnAboutLosingChangesTo:ifCancel:ifMerge:) ast link: link
]

{ #category : #baseline }
BaselineOfSpec >> baseline: spec [
	<baseline>

	spec for: #common do: [ 
		spec 
			preLoadDoIt: #addHackToLoadSpec;
			postLoadDoIt: #removeHackToLoadSpec.
			
		spec 
			baseline: 'ParametrizedTests' 
			with: [ 
				spec
					repository: 'github://tesonep/ParametrizedTests/src';
  					loads: 'default' ].

		spec 
			package: 'Spec-Core' with: [ spec requires: #('Spec-Layout') ];
			package: 'Spec-Deprecated80'with: [ spec requires: #('Spec-Tests' 'Spec-MorphicAdapters') ];
			package: 'Spec-Inspector'with: [ spec requires: #('Spec-PolyWidgets') ];
			package: 'Spec-Layout';
			package: 'Spec-MorphicAdapters' with: [ spec requires: #('Spec-Core') ];
			package: 'Spec-PolyWidgets' with: [ spec requires: #('Spec-Core') ];
			package: 'Spec-Tools' with: [ spec requires: #('Spec-PolyWidgets') ];
			package: 'Spec-StubAdapter' with: [ spec requires: #('Spec-Core') ];
			package: 'Spec-Examples' with: [ spec requires: #('Spec-Tools' 'Spec-Inspector') ];
			package: 'Spec-Tests' with: [ spec requires: #('Spec-PolyWidgets' 'Spec-Examples') ];
			package: 'Spec-MorphicBackendTests' with: [ spec requires: #( 'Spec-MorphicAdapters' 'ParametrizedTests') ];
			package: 'Spec-CompatibilityTests' with: [ spec requires: #( 'Spec-MorphicBackendTests') ] ].
	spec for: #'pharo7.x' do: [ 
		spec package: 'Spec-Pharo7To8Compatibility' ]
]

{ #category : #'pre/post load' }
BaselineOfSpec >> removeHackToLoadSpec [
	
	self shouldInstallHack ifFalse: [ ^ self ].
	
	link uninstall
]

{ #category : #'pre/post load' }
BaselineOfSpec >> shouldInstallHack [
	^ {CommandLineUIManager. NonInteractiveUIManager} anySatisfy: [ :e | UIManager default isKindOf: e ]
]
