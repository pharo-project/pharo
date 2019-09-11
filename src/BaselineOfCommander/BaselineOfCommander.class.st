Class {
	#name : #BaselineOfCommander,
	#superclass : #BaselineOf,
	#category : #BaselineOfCommander
}

{ #category : #baselines }
BaselineOfCommander >> baseline: spec [
	<baseline>
	
	| repository | 	
	repository := self packageRepositoryURL.
	
	spec for: #'common' do: [
		spec
			baseline: #ClassAnnotation with: [ 
				spec 
					repository: repository;
					loads: 'Core' ];
			project: #ClassAnnotationTests copyFrom: #ClassAnnotation with: [
				spec loads: 'Tests'];
			package: #'Commander-Core' with: [spec requires: #(ClassAnnotation)];
			package: #'Commander-Core-Tests' with: [
				spec requires: #(#'Commander-Core' #ClassAnnotationTests)];
	
			package: #'Commander-Activators-Shortcut' with: [
				spec requires: #(#'Commander-Core' )];
			package: #'Commander-Activators-ContextMenu' with: [
				spec requires: #(#'Commander-Core' #'Commander-Activators-Shortcut' )];
			package: #'Commander-Activators-DragAndDrop' with: [
				spec requires: #(#'Commander-Core' )];

			package: #'Commander-Activators-WorldMenu' with: [
				spec requires: #(#'Commander-Activators-Shortcut' #'Commander-Activators-ContextMenu' )];
			package: #'Commander-Activators-Mouse' with: [
				spec requires: #(#'Commander-Core' )];
			package: #'Commander-Activators-TextView' with: [
				spec requires: #(#'Commander-Activators-ContextMenu' #'Commander-Activators-Mouse')];
			
			package: #'Commander-SpecSupport';
			package: #'Commander-Spec2-Compatibility' with: [spec requires: #(#'Commander-Core' )];			
			package: #'Commander-Examples' with: [
				spec requires: #(#'Commander-SpecSupport')].
		spec 
			group: 'Core' with: #(#'Commander-Core');
			group: 'AllActivators' with: #(#'Commander-Activators-Shortcut' #'Commander-Activators-ContextMenu' #'Commander-Activators-DragAndDrop' #'Commander-Activators-WorldMenu' #'Commander-Activators-TextView' #'Commander-Activators-Mouse');
			group: 'Tests' with: #(#'Commander-Core-Tests' );
			group: 'default' with: #('Core' 'AllActivators' 'Tests' 'Commander-SpecSupport' 'Commander-Spec2-Compatibility') ]
]
