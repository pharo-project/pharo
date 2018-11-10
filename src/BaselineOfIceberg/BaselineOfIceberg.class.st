Class {
	#name : #BaselineOfIceberg,
	#superclass : #BaselineOf,
	#category : #BaselineOfIceberg
}

{ #category : #baseline }
BaselineOfIceberg >> baseline: spec [
	<baseline>
	
	spec for: #common do: [ 
		spec postLoadDoIt: #postLoadWithDirective:projectSpec:.
		
		self tonel: spec.
		self libgit: spec.
		self commander: spec.
		
		spec
			baseline: 'Ring2' 
			with: [ spec repository: 'github://pavel-krivanek/Ring2:mergeDev/src' ].
		
		spec
			"iceberg"
			package: 'Iceberg';
			package: 'Iceberg-Plugin' with: [ spec requires: #('Iceberg' 'Iceberg-TipUI' 'Commander') ];
			package: 'Iceberg-Plugin-Metacello' with: [ spec requires: #('Iceberg-Plugin') ];
			package: 'Iceberg-Plugin-GitHub' with: [ spec requires: #('Iceberg-Plugin') ];
			package: 'Iceberg-Plugin-Pharo' with: [ spec requires: #('Iceberg-Plugin') ];
			package: 'Iceberg-Plugin-Migration' with: [ spec requires: #('Iceberg-Plugin') ];
			package: 'Iceberg-Metacello-Integration' with: [ spec requires: #('Iceberg')];
			package: 'Iceberg-TipUI' with: [ spec requires: #('Iceberg' 'Commander') ];
			package: 'Iceberg-Memory' with: [ spec requires: #('Iceberg') ];
			package: 'Iceberg-Tests' with: [ spec requires: #('Iceberg' 'Iceberg-GitCommand' 'Iceberg-Memory' 'Ring2') ];
			package: 'Iceberg-Tests-MetacelloIntegration' with: [ spec requires: #('Iceberg-Tests') ];
			package: 'Iceberg-UI-Tests' with: [ spec requires: #('Iceberg-TipUI' 'Iceberg-Tests')];
			package: 'Iceberg-Plugin-Migration-Tests' with: [ spec requires: #('Iceberg-Plugin-Migration' 'Iceberg-Tests') ];
			"libgit"
			package: 'Iceberg-Libgit' with: [ spec requires: #('Iceberg' 'LibGit') ];
			package: 'Iceberg-Libgit-Filetree' with: [ spec requires: #('Iceberg' 'Iceberg-Libgit') ];
			package: 'Iceberg-Libgit-Tonel' with: [ spec requires: #('Iceberg' 'Iceberg-Libgit' 'Tonel') ];
			"for testing"
			for: #MacOS do: [ 
				self osSubprocess: spec.
				spec package: 'Iceberg-GitCommand' with: [ spec requires: #('OSSubprocess') ] ];
			for: #Unix do: [ 
				self osSubprocess: spec.
				spec package: 'Iceberg-GitCommand' with: [ spec requires: #('OSSubprocess' ) ] ];
			for: #Windows do: [ 
				self processWrapper: spec.
				spec package: 'Iceberg-GitCommand' with: [ spec requires: #('ProcessWrapper') ] ];
			"groups"
			group: 'default' with: #(
				minimal 
				'Iceberg-Metacello-Integration' 
				'Iceberg-TipUI'
				'Iceberg-Plugin' 
				'Iceberg-Plugin-Metacello'
				'Iceberg-Plugin-GitHub'
        'Iceberg-Plugin-Pharo'
        'Iceberg-Plugin-Migration');
			group: 'minimal' with: #(
				'Iceberg'
				'Iceberg-Libgit' 
				'Iceberg-Libgit-Tonel'
				'Iceberg-Libgit-Filetree');
			group: 'allTests' with: #(
				'Iceberg-Tests' 
				'Iceberg-Tests-MetacelloIntegration'
				'LibGit-Tests' 
				'Iceberg-UI-Tests'
				'Iceberg-Plugin-Migration-Tests' );
			group: 'development' with: #(default allTests) ].

	spec for: #'pharo6.x' do: [	
		self spec70: spec.
		spec
			package: 'Iceberg' with: [ 
				spec requires: #( 'Iceberg-Pharo6' ) ];
			package: 'Iceberg-Pharo6';
			package: 'Iceberg-TipUI' with: [ 
				spec requires: #('Spec70Compatibility' 'Iceberg-Pharo6') ] ]
]

{ #category : #baseline }
BaselineOfIceberg >> commander: spec [
	spec 
		baseline: 'Commander' 
		with: [ 
			spec 
				repository: 'github://pharo-ide/Commander:v0.6.3';
				loads: #('Core' 'AllActivators' 'Commander-SpecSupport') ]
]

{ #category : #accessing }
BaselineOfIceberg >> customProjectAttributes [
	Smalltalk os isMacOS ifTrue: [ ^ #(#MacOS) ].
	Smalltalk os isUnix ifTrue: [ ^ #(#Unix) ].
	Smalltalk os isWindows ifTrue: [ ^ #(#Windows) ]
]

{ #category : #baseline }
BaselineOfIceberg >> libgit: spec [
	spec 
		baseline: 'LibGit' 
		with: [ 
			spec
				repository: 'github://pharo-vcs/libgit2-pharo-bindings:v1.5.4';
  				loads: 'default' ].
	spec  
		project: 'LibGit-Tests'
		copyFrom: 'LibGit'
		with: [ spec loads: #('tests') ]
]

{ #category : #baseline }
BaselineOfIceberg >> osSubprocess: spec [
	spec 
		baseline: 'OSSubprocess' 
		with: [ spec repository: 'github://pharo-contributions/OSSubprocess:v1.0.1/repository' ]

]

{ #category : #script }
BaselineOfIceberg >> postLoad [

	self resetKMRepository.
	(Smalltalk at: #Iceberg) bootstrap
]

{ #category : #script }
BaselineOfIceberg >> postLoadWithDirective: aDirective projectSpec: aSpec [
	| repository |
	self resetKMRepository.

	"Getting the repository from metacello..."
	repository := aSpec repositories list first spec createRepository.

	"We only apply the bootstrap only of the repository is a github repository.
	This is because in Iceberg's CI, the repository is cloned before and the code is loaded using filetree, so we have no commit information here..."
	(repository isKindOf: MCGitHubRepository) ifFalse: [ ^ self ].

	(Smalltalk at: #Iceberg)
		bootstrapWithCommitId: repository commitId
		packageList: aSpec packageNames
]

{ #category : #baseline }
BaselineOfIceberg >> processWrapper: spec [
	spec 
		configuration: 'ProcessWrapper' 
		with: [ 
			spec
				versionString: '1.2';
				repository: 'http://smalltalkhub.com/mc/hernan/ProcessWrapper/main' ]				
]

{ #category : #accessing }
BaselineOfIceberg >> projectClass [
	^ super projectClass 
	"this is breaking the bootstrap. Reverting it for now."
	"[ self class environment at: #MetacelloCypressBaselineProject ]
		on: NotFound
		do: [ super projectClass ]"
]

{ #category : #script }
BaselineOfIceberg >> resetKMRepository [
	Smalltalk 
		at: #KMRepository 
		ifPresent: [ :class | class reset ]
]

{ #category : #baseline }
BaselineOfIceberg >> spec70: spec [
	"for pharo6 compatibility"
	spec
		baseline: 'Spec70Compatibility' 
		with: [ spec repository: 'github://pharo-contributions/Spec70Compatibility:v1.0.0/src' ]
]

{ #category : #baseline }
BaselineOfIceberg >> tonel: spec [
	spec
		baseline: 'Tonel' 
		with: [ spec repository: 'github://pharo-vcs/tonel:v1.0.9' ]
]
