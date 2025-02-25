"
I'm a tree node representing a method implementation
"
Class {
	#name : 'DAMethodImplementationNode',
	#superclass : 'DAMethodDependencyNode',
	#category : 'Tool-DependencyAnalyser-UI-Nodes',
	#package : 'Tool-DependencyAnalyser-UI',
	#tag : 'Nodes'
}

{ #category : 'building-menu' }
DAMethodImplementationNode >> addMenuActionsOn: anActionGroup [
	| method |
	
	method := self content.
	anActionGroup addActionWith: [ :action | action
		name: 'Select this package as the right dependency';
		action: [
			(TheManifestBuilder of: self packageUnderAnalysis packageManifest)
				addManuallyResolvedDependency: method package name.
				"TODO: run the analysis again" ] ]
]

{ #category : 'accessing' }
DAMethodImplementationNode >> icon [
	^ self iconNamed: #package
]

{ #category : 'displaying' }
DAMethodImplementationNode >> label [
	^ String streamContents: [ :stream |
		stream
			nextPutAll: 'Implemented in ';
			nextPutAll: self content package name;
			nextPutAll: ': #';
			nextPutAll: self content methodClass name ]
]
