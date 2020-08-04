"
Microdown is a minimal but extensible markup language. It is kind of compatible with githubmarkdown for a core set of feature. 
Read comment of MicroDownParser. 
"
Class {
	#name : #BaselineOfMicrodown,
	#superclass : #BaselineOf,
	#category : #BaselineOfMicrodown
}

{ #category : #baselines }
BaselineOfMicrodown >> baseline: spec [
	<baseline>
	spec for: #'common' do: [
		spec baseline: 'Pillar' with: [ spec 
				repository: self packageRepositoryURL ].
		spec 
			package: #'Microdown';
			package: #'Microdown-Tests'  with: [
				spec requires:  #( #'Microdown') ];
			
			package: #'Microdown-Pillar' with: [
				spec requires:  #( #'Microdown' 'Pillar') ];
			package: #'Microdown-Pillar-Tests' with: [
				spec requires:  #(#'Microdown-Pillar' #'Microdown-Tests') ];
			package: #'Microdown-Calypso' with: [
				spec requires:  #(#'Microdown-Pillar' 'Microdown-RichTextComposer') ];
			package: #'Microdown-RichTextComposer' with: [
				spec requires:  #('Microdown' #'Microdown-Pillar') ]
			]

]
