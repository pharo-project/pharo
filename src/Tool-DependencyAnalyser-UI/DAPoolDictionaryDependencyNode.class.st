"
I am a tree node showing pool dictionary dependencies.
I define an icon for pool dictionaries and spawn a new browser on the pool dictionary when browsing the dependency.
"
Class {
	#name : #DAPoolDictionaryDependencyNode,
	#superclass : #DAClassNode,
	#category : #'Tool-DependencyAnalyser-UI-Nodes'
}

{ #category : #accessing }
DAPoolDictionaryDependencyNode >> icon [

	^ self iconNamed: #classVarsSelected
]

{ #category : #'browse-nautilus' }
DAPoolDictionaryDependencyNode >> spawnNewBrowser [
	self browseClass: self content poolDictionary
]
